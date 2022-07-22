# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## This is the function that processes the yaml and reprojects and crops some rasters
## inpJSON is the JSON string passed by Lambda to the function, and must conform to the example here: https://pointblue.atlassian.net/wiki/spaces/RW/pages/1505525765/Processing+S3+trigger+event+using+R
## The yaml file must be like the example here:
batchImportRasters_toOSW<-function(inpJSON){
	
	## supress warnings
	options(warn=-1)
	
	## libraries an other needes
	libs<-c("raster","rgdal","sp","airtabler","yaml","jsonlite","aws.s3","remotes")
	w<-try(suppressPackageStartupMessages(sapply(libs, require, character.only=T)),silent=T) #check
	
	## Are we missing a needed library?
	if(FALSE %in% w){
		wdf<-data.frame(Lib=names(w),LoadStatus=as.character(w));row.names(wdf)<-NULL
		jwdf<-jsonlite::toJSON(subset(wdf,LoadStatus=="FALSE"))
		retJSON<-paste0("{\"Error\":[{\"Process\":\"Loading libraries\",\"value\":\"Missing libraries\"}], \"Description\":\"",jwdf,"\"}")
		return(retJSON)
	}
		
	## Is the input a valid JSON string?
	if(!is.character(inpJSON)){
		retJSON<-"{\"Error\":[{\"Process\":\"Reading JSON\",\"value\":\"Failed to load JSON\"}], \"Description\":\"Input is not a character string\"}"
		return(retJSON)
	}
	if(!jsonlite::validate(inpJSON)[[1]]){
		retJSON<-"{\"Error\":[{\"Process\":\"Reading JSON\",\"value\":\"Failed to load JSON\"}], \"Description\":\"Invalid JSON\"}"
		return(retJSON)
	}
	
	#We have a valid JSON
	inplst<-jsonlite::fromJSON(inpJSON)
	
	## Delete if container role provides access
	Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIATK6LYMII6FUMM5QO",
			"AWS_SECRET_ACCESS_KEY" = "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",
			"AWS_DEFAULT_REGION" = "us-west-2")
	
	# Assert:
	#Can connect to bucket and find yaml (i.e., bucket exists)?
	bktt<-aws.s3::bucket_exists("offshore-wind-data")
	if(!bktt[1]){
		retJSON<-"{\"Error\":[{\"Process\":\"Bucket access\",\"value\":\"Failed to access the bucket\"}], \"Description\":\"Failed to validate existence of bucket offshore-wind-data\"}"
		return(retJSON)
	}
	
	#Can get bucket data. Is the yaml key valid?
	inpYAML<-inplst$Records$s3$object$key[[1]][1]  #This should be the full path to the yaml, right???
	if(nchar(inpYAML)<20 | !file.exists(inpYAML)){
		retJSON<-"{\"Error\":[{\"Process\":\"Read JSON key\",\"value\":\"Invalid path to yaml file\"}], \"Description\":\"The key must contain a valid name and full path to the yaml file in the bucket\"}"
		return(retJSON)
	}
	
	## Is the input a valid yaml document?
	ymlc<-try(yaml::read_yaml(inpYAML),silent=TRUE)
	if(inherits(ymlc,"try-error")){
		errmsg<-toJSON(ymlc)
		retJSON<-paste0("{\"Error\":[{\"Process\":\"Read yaml\",\"value\":\"Failed to read\"}], \"Description\":\"",errmsg,"\"}")
		return(retJSON)
	}
	
	# Validate yaml contents - must have:
	# AT key and db key
	atKP<-ymlc$ATkeypair
	# Target AT table
	atTable<-ymlc$ATtable
	chkTable<-atTable %in% c("Seabirds","MarineMammalsTurtles","Fish","Benthic","HumanUses")
	# Target species
	sciName<-ymlc$sciName
	chkSpp<-ifelse(sciName=="",FALSE,TRUE)
	# season (default all)
	chkSeasons<-toupper(ymlc$seasons) %in% c("",NULL,NA,"ALL","SUMMER","FALL","WINTER","SPRING")
	allseasons<-c("Summer","Fall","Winter","Spring")
	seasons<-ifelse(ymlc$seasons=="",allseasons,ifelse(is.na(ymlc$seasons),allseasons,ifelse(is.null(ymlc$seasons),allseasons,ifelse(toupper(ymlc$seasons)=="ALL",allseasons,ymlc$seasons))))
		
	### READ in bucket using aws.s3 command
	# raster path and name inside the bucket
	inpRast<-ymlc$inputRaster
	chkRast<-file.exists(inpRast)
	# base grid path and name
	inpGrid<-ymlc$baseGridPathName
	chkGrid<-file.exists(inpGrid)
	# savepath - where to save the re-projected raster
	savePath<-ymlc$saveDir
	chkSavePath<-dir.exists(savePath)
	# logdir - where to create a log file for this execution
	logDir<-ymlc$logDir
	chkLogDir<-dir.exists(logDir)
	# logSessInfo - include session info in log? (default N)
	logSessionInfo<-ifelse(toupper(ymlc$logSessionInfo)=="Y",TRUE,FALSE)
	
	## Did the YAML document provide all the needed inputs?
	if(nchar(atKP)!=35 | !chkTable | !chkSpp | !chkRast | !chkGrid | !chkSeasons | !chkSavePath | !chkLogDir){
		retJSON<-paste("{\"Error\":[{\"Process\":\"Validate yaml\",\"value\":\"Missing or wrong mandatory inputs\"}],", 
				"\"Description\":\"One or more of the following are wrong or missing in the input yaml: AirTable key pair, target table or species,",
				"input raster or base grid filepath, a valid season, output directory path, log directory path. \"}")
		return(retJSON)
	}
		
	## functions we will need
	##################################
	## This is the Airtable data query function
	# base is "NeededDistData"
	# tblnam is the table name, one of: "Seabirds","MarineMammalsTurtles", "Fish", "Benthic", "HumanUses"
	getATtable<-function(base="NeededDistData",tblnam,includeID=FALSE){
		baseID<-"appO3EgtQh3UREeun"
		rcweights<-airtable(base = baseID, table = tblnam)
		rcw<-rcweights[[tblnam]]$select_all()
		if(!includeID){
			rcw<-rcw[,which(!names(rcw) %in% c("id","createdTime"))]
		}else{
			rcw<-rcw[,which(!names(rcw) %in% c("createdTime"))]
		}
		
		return(rcw)
	}	
	
	## tablesToProcess returns a data.frame naming all the species, dataset path, layer name and season for each table to process
	## It retrieves this information from AirTable - for only one species and one or all seasons
	# atSourceTable is the atTable parameter in the yaml
	getTablesToProcess<-function(atSourceTable,seasons){
		sourcesTable<-getATtable(base="NeededDistData",tblnam=atSourceTable,includeID=TRUE) #catch
		
		tablesList<-list()
		if("Summer" %in% seasons){tablesList[["Summer"]]<-sourcesTable[,c("id","Species","SummerPathToDataset","SummerLayerName")]}
		if("Fall" %in% seasons){tablesList[["Fall"]]<-sourcesTable[,c("id","Species","FallPathToDataset","FallLayerName")]}
		if("Winter" %in% seasons){tablesList[["Winter"]]<-sourcesTable[,c("id","Species","WinterPathToDataset","WinterLayerName")]}
		if("Spring" %in% seasons){tablesList[["Spring"]]<-sourcesTable[,c("id","Species","SpringPathToDataset","SpringLayerName")]}
		
		for(tt in names(tablesList)){
			sourcesTable<-tablesList[[tt]]
			names(sourcesTable)<-c("recId","Species","datasetName","layerName")
			sourcesTable<-subset(sourcesTable,!is.na(datasetName))
			sourcesTable$Season<-tt
			tablesList[[tt]]<-sourcesTable
		}
		
		tablesToProcess<-rbindlist(tablesList) #catch
		tablesToProcess<-unique(tablesToProcess)
		
		return(tablesToProcess)
	}
	##########################################
	
	## Load the grid - this is the reference grid to rasterize to - path in YAML
	aliqgrd<-raster(inpGrid) #exists, catch
	projAliq<-projection(aliqgrd)
	tgEx<-as.matrix(extent(aliqgrd))
	targExt<-paste("xmin:",tgEx[1,1]," xmax:",tgEx[1,2]," ymin:",tgEx[2,1]," ymax:",tgEx[2,2]) #log
	
	## BIG QUESTION:
	## How do we know that the input data are indeed related to these tables? We trust the user.
	#generate the list of tables to be processed
	tablesToProcess<-try(getTablesToProcess(atSourceTable=atTable,seasons=seasons),silent=TRUE) 
	if(inherits(tablesToProcess,"try-error") | nrow(tablesToProcess)==0){
		errmsg<toJSON(tablesToProcess)
		retJSON<-paste0("{\"Error\":[{\"Process\":\"List tables to process\",\"value\":\"Failed to create list\"}], \"Description\":\"",errmsg,"\"}")
		return(retJSON)
	}
	
	## READY TO PROCESS
	## reproject and update AT
	updateResults<-data.frame()
	
	### Loop through each layer and reproject and extract to the Aliquote grid - this is what could be parallelized
	#for(ii in 1:nrow(tablesToProcess)){
	#	#targets (generated by the function tablesToPocess
	#	datasetNm<-as.character(tablesToProcess[ii,"datasetName"]) 
	#	layerNm<-as.character(tablesToProcess[ii,"layerName"]) 
	#	season<-as.character(tablesToProcess[ii,"Season"]) 
	#	rastLayer<-try(raster(inpRast),silent=TRUE)
	#	
	#	if(!inherits(rastLayer,"try-error")){
	#		origProj<-projection(rastLayer)
	#		orEx<-as.matrix(extent(rastLayer))
	#		origExt<-paste("xmin:",orEx[1,1]," xmax:",orEx[1,2]," ymin:",orEx[2,1]," ymax:",orEx[2,2])
	#		origExtent<-origExt
	#		
	#		sppTable<-try(projectRaster(from=rastLayer, to=aliqgrd),silent=TRUE)							
	#
	#		## HERE crop to the EEZ - make it optional in the YAML  IFF projectRaster output is not size and extent of aliquote grid
	#		
	#		if(!inherits(sppTable,"try-error")){
	#			spptable<-as.data.frame(sppTable)
	#			spptable$gridCellId<-1:nrow(spptable)
	#			## Collect metadata
	#			dateCreated<-format(Sys.time(),"%Y-%m-%d %H:%M")
	#			metadf<-data.frame(SourceGeodatabase=datasetNm,
	#					SourceLayer=layerNm,
	#					SourceProjection=origProj,
	#					TargetProjection=projAliq,
	#					SourceExtent=origExtent,
	#					TargetExtent=targExt,
	#					AggregationFunction="bilinear",
	#					DateCreated=dateCreated
	#			)
	#			
	#			## Save
	#			savefile<-paste0(savePath,layerNm,".RData") 
	#			kk<-try(save(spptable,metadf,file=savefile),silent=TRUE)
	#			
	#			## Tabulate date of creation
	#			recId<-as.character(tablesToProcess[ii,"recId"])
	#			season<-as.character(tablesToProcess[ii,"Season"])
	#			dateVal<-format(Sys.time(),"%Y-%m-%d %H:%M")
	#			if(season=="Summer"){  #Can only be one of four...
	#				newdata<-list(SummerTableVer=dateVal)
	#			}else if(season=="Fall"){
	#				newdata<-list(FallTableVer=dateVal)
	#			}else if(season=="Winter"){
	#				newdata<-list(WinterTableVer=dateVal)
	#			}else{
	#				newdata<-list(SpringTableVer=dateVal)
	#			}
	#			
	#			#Update the AT to append the date
	#			appendtry<-try(air_update(base="appO3EgtQh3UREeun",table_name=atSourceTable,
	#							record_id=recId,record_data=newdata),silent=TRUE)
	#			if(inherits(appendtry,"try-error")){
	#				tdf<-data.frame(Base=datasetNm,Layer=layerNm,Season=season,
	#						ReprojectionResult="Failed to update metadata on raster in AirTable",
	#						ATupdateResult=paste(appendtry,collapse="; "))
	#			}else{
	#				tdf<-data.frame(Base=datasetNm,Layer=layerNm,Season=season,
	#						ReprojectionResult="Successfully reprojected and saved",
	#						ATupdateResult="Successfully updated AT record")
	#			}
	#		}else{
	#			tdf<-data.frame(Base=datasetNm,Layer=layerNm,Season=season, 
	#					ReprojectionResult=paste(sppTable, collapse="; "),
	#					ATupdateResult="Failed to re-project raster to aliquote grid")
	#		}
	#	}else{ #failed to read the raster layer
	#		tdf<-data.frame(Base=datasetNm,Layer=layerNm,Season=season,
	#				ReprojectionResult="Failed to load the raster layer",
	#				ATupdateResult=paste(rastLayer,collapse="; "))
	#	}
	#	updateResults<-rbind(updateResults,tdf)
	#}
	#retJSON<-toJSON(updateResults)	
	retJSON<-paste0("{\"Success\":[{\"Process\":\"Everything but processing\",\"value\":\"Checks passed\"}], \"Description\":\"All good\"}")
	return(retJSON)
	
}




# TODO: 
# 
# Author: lsalas
###############################################################################


## This is the function that processes the yaml and reprojects and crops some rasters
## inpJSON is the JSON string passed by Lambda to the function, and must conform to the example here: https://pointblue.atlassian.net/wiki/spaces/RW/pages/1505525765/Processing+S3+trigger+event+using+R
## The yaml file must be like the example here:
batchImportRasters_toOSW<-function(inpJSON){
	
	#start log timing here...
	logstart<-format(Sys.time(), "%Y%m%d %H%M%S")
	
	## supress warnings
	options(warn=-1)
	
	## libraries an other needes
	libs<-c("raster","rgdal","sp","airtabler","yaml","jsonlite","aws.s3","remotes","data.table")
	w<-try(suppressPackageStartupMessages(sapply(libs, require, character.only=T)),silent=T) #check
	
	## Are we missing a needed library?
	if(FALSE %in% w){
		wdf<-data.frame(Lib=names(w),LoadStatus=as.character(w));row.names(wdf)<-NULL
		jwdf<-jsonlite::toJSON(subset(wdf,LoadStatus=="FALSE"))
		retJSON<-paste0("{\"Out\":[{\"Result\":\"Error\",\"Process\":\"Loading libraries\",\"value\":\"Missing libraries\"}], \"Description\":\"",jwdf,"\"}")
		return(retJSON)
	}
		
	## Is the input a valid JSON string?
	if(!is.character(inpJSON)){
		retJSON<-"{\"Out\":[{\"Result\":\"Error\",\"Process\":\"Reading JSON\",\"value\":\"Failed to load JSON\"}], \"Description\":\"Input is not a character string\"}"
		return(retJSON)
	}
	if(!jsonlite::validate(inpJSON)[[1]]){
		retJSON<-"{\"Out\":[{\"Result\":\"Error\",\"Process\":\"Reading JSON\",\"value\":\"Failed to load JSON\"}], \"Description\":\"Invalid JSON\"}"
		return(retJSON)
	}
	
	#We have a valid JSON
	inplst<-jsonlite::fromJSON(inpJSON)
	
	# Assert:
	#Can connect to bucket and find yaml (i.e., bucket exists)?
	inpBuck<-"offshore-wind-data"
	bktt<-aws.s3::bucket_exists(inpBuck)
	if(!bktt[1]){
		retJSON<-"{\"Out\":[{\"Result\":\"Error\",\"Process\":\"Bucket access\",\"value\":\"Failed to access the bucket\"}], \"Description\":\"Failed to validate existence of bucket offshore-wind-data\"}"
		return(retJSON)
	}
	
	# The yaml exists, because it triggered the event. Can we read it? Is the yaml valid?
	inpYAML<-inplst$Records$s3$object$key[[1]][1]  #This should be the full path to the yaml, right???
	if(nchar(inpYAML)<20){
		retJSON<-"{\"Out\":[{\"Result\":\"Error\",\"Process\":\"Read JSON key\",\"value\":\"Invalid path to yaml file\"}], \"Description\":\"The key must contain a valid name and full path to the yaml file in the bucket\"}"
		return(retJSON)
	}
	
	#  Can we load it? Is the input a valid yaml document?
	chryaml<-rawToChar(get_object(bucket="offshore-wind-data",object=inpYAML))
	ymlc<-try(yaml::yaml.load(chryaml),silent=TRUE)
	if(inherits(ymlc,"try-error")){
		errmsg<-toJSON(ymlc)
		retJSON<-paste0("{\"Out\":[{\"Result\":\"Error\",\"Process\":\"Read yaml\",\"value\":\"Failed to read\"}], \"Description\":\"",errmsg,"\"}")
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
	chkRast<-suppressMessages(object_exists(bucket=inpBuck,object=inpRast))
	# base grid path and name
	inpGrid<-ymlc$baseGridPathName
	chkGrid<-suppressMessages(object_exists(bucket=inpBuck,object=inpGrid))
	# savepath - where to save the re-projected raster
	savePath<-ymlc$saveDir
	chkSavePath<-suppressMessages(object_exists(bucket=inpBuck,object=savePath))
	# logdir - where to create a log file for this execution
	logDir<-ymlc$logDir
	chkLogDir<-suppressMessages(object_exists(bucket=inpBuck,object=logDir))
	# logSessInfo - include session info in log? (default N)
	logSessionInfo<-ifelse(toupper(ymlc$logSessionInfo)=="Y",TRUE,FALSE)
	
	## Did the YAML document provide all the needed inputs?
	if(nchar(atKP)!=35 | !chkTable | !chkSpp | !chkRast | !chkGrid | !chkSeasons | !chkSavePath | !chkLogDir){
		retJSON<-paste("{\"Out\":[{\"Result\":\"Error\",\"Process\":\"Validate yaml\",\"value\":\"Missing or wrong mandatory inputs\"}],", 
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
		if("Summer" %in% seasons){tablesList[["Summer"]]<-sourcesTable[,c("id","ScientificName","SummerPathToDataset","SummerLayerName")]}
		if("Fall" %in% seasons){tablesList[["Fall"]]<-sourcesTable[,c("id","ScientificName","FallPathToDataset","FallLayerName")]}
		if("Winter" %in% seasons){tablesList[["Winter"]]<-sourcesTable[,c("id","ScientificName","WinterPathToDataset","WinterLayerName")]}
		if("Spring" %in% seasons){tablesList[["Spring"]]<-sourcesTable[,c("id","ScientificName","SpringPathToDataset","SpringLayerName")]}
		
		for(tt in names(tablesList)){
			sourcesTable<-tablesList[[tt]]
			names(sourcesTable)<-c("recId","ScientificName","datasetName","layerName")
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
	# Check the local temp directory:
	localTempDir<-ymlc$localTempDir
	ltdchk<-dir.exists(localTempDir)
	if(!ltdchk){
		retJSON<-"{\"Out\":[{\"Result\":\"Error\",\"Process\":\"Access local temp directory\",\"value\":\"Invalid path to local temp directory\"}], \"Description\":\"A valid and accessible local temp directory must be provided\"}"
		return(retJSON)
	}
	
	#create log file for this run:
	filelogn<-paste0("OSW_CA_batchRasterProcessing_",logstart)
	logfile<-paste(localTempDir,filelogn,".log",sep="")
	savelog<-paste0(logDir,filelogn,".log")
	sitd<-as.character(sessionInfo())
	
	zz <- try(file(logfile, "w"),silent=T)
	flog<-0
	if(inherits(zz,"try-error")){
		retJSON<-"{\"Out\":[{\"Result\":\"Error\",\"Process\":\"Open local log file\",\"value\":\"Failed to create log file in local temp directory\"}], \"Description\":\"Could not open log file in local directory. Logs will not be kept.\"}"
		flog<-1
	}
	if(flog==0){
		cat("Log batch raster processing script", paste("Started", logstart), file = zz, sep = "\n", append=TRUE)
		cat("\n","\n",file = zz, append=TRUE)
		cat("All libraries required are loaded.",sep ="\n",file = zz, append=TRUE)
		cat("Valid JSON string provided.",sep ="\n",file = zz, append=TRUE)
		cat(paste("Processing YAML file:",inpYAML),sep ="\n",file = zz, append=TRUE)
		cat("YAML contains valid and sufficient input.",sep ="\n",file = zz, append=TRUE)
	}
	
	# copy raster data locally from bucket
	wgrd<-try(save_object(object="FirstProcess/AliquoteGrid/aliqgrid.grd",bucket="offshore-wind-data",file=paste0(localTempDir,"aliqgrid.grd")),silent=T)
	wgri<-try(save_object(object="FirstProcess/AliquoteGrid/aliqgrid.gri",bucket="offshore-wind-data",file=paste0(localTempDir,"aliqgrid.gri")),silent=T)
	if(inherits(wgrd,"try-error") | inherits(wgri,"try-error"))
	if(!inherits(wgrd,"try-error") & !inherits(wgri,"try-error")){
		retJSON<-"{\"Out\":[{\"Result\":\"Error\",\"Process\":\"Copy base grid into temp directory\",\"value\":\"Failed\"}], \"Description\":\"Failed to copy base grid into the local temp directory\"}"
		if(flog==0){
			cat("Failed to copy base grid locally.",sep ="\n",file = zz, append=TRUE)
			if(logSessionInfo){
				cat(sitd,sep="\n",file=zz,append=TRUE)
			}
			cat("\n","\n",file = zz, append=TRUE)
			cat("Terminating batch raster processing.",sep ="\n",file = zz, append=TRUE)
			close(zz)
			svlg<-try(put_object(file=logfile,object=savelog,bucket="offshore-wind-data"),silent=TRUE)
		}
		return(retJSON)
	}
	fegrd<-file.exists(paste0(localTempDir,"aliqgrid.grd"))
	fegri<-file.exists(paste0(localTempDir,"aliqgrid.grd"))
	if(!fegrd | !fegri){
		retJSON<-"{\"Out\":[{\"Result\":\"Error\",\"Process\":\"Copy base grid into temp directory\",\"value\":\"Failed\"}], \"Description\":\"Failed to copy base grid into the local temp directory\"}"
		if(flog==0){
			cat("Failed to copy base grid locally.",sep ="\n",file = zz, append=TRUE)
			if(logSessionInfo){
				cat(sitd,sep="\n",file=zz,append=TRUE)
			}
			cat("\n","\n",file = zz, append=TRUE)
			cat("Terminating batch raster processing.",sep ="\n",file = zz, append=TRUE)
			close(zz)
			svlg<-try(put_object(file=logfile,object=savelog,bucket="offshore-wind-data"),silent=TRUE)
		}
		return(retJSON)
	}

	aliqgrd<-try(raster(paste0(localTempDir,"aliqgrid.gri")),silent=T)
	if(inherits(aliqgrd,"try-error")){
		retJSON<-"{\"Out\":[{\"Result\":\"Error\",\"Process\":\"Load raster\",\"value\":\"Error loading raster\"}], \"Description\":\"The raster was copied locally and is accessible, but failed to load. Corrupt raster file?\"}"
		if(flog==0){
			cat("Failed to load the base grid into a raster.",sep ="\n",file = zz, append=TRUE)
			if(logSessionInfo){
				cat(sitd,sep="\n",file=zz,append=TRUE)
			}
			cat("\n","\n",file = zz, append=TRUE)
			cat("Terminating batch raster processing.",sep ="\n",file = zz, append=TRUE)
			close(zz)
			svlg<-try(put_object(file=logfile,object=savelog,bucket="offshore-wind-data"),silent=TRUE)
		}
		return(retJSON)
	}
	projAliq<-projection(aliqgrd)
	tgEx<-as.matrix(extent(aliqgrd))
	targExt<-paste("xmin:",tgEx[1,1]," xmax:",tgEx[1,2]," ymin:",tgEx[2,1]," ymax:",tgEx[2,2]) #log
	if(flog==0){
		cat("Loaded base grid into raster.",sep ="\n",file = zz, append=TRUE)
	}
	
	## BIG QUESTION:
	## How do we know that the input data are indeed related to these tables? We trust the user.
	#generate the list of tables to be processed
	# First need to set the env key:
	Sys.setenv(AIRTABLE_API_KEY=strsplit(atKP,":")[[1]][1])
	tablesToProcess<-try(getTablesToProcess(atSourceTable=atTable,seasons=seasons),silent=TRUE) 
	if(inherits(tablesToProcess,"try-error") | nrow(tablesToProcess)==0){
		errmsg<-toJSON(tablesToProcess)
		retJSON<-paste0("{\"Out\":[{\"Result\":\"Error\",\"Process\":\"List tables to process\",\"value\":\"Failed to create list\"}], \"Description\":\"",errmsg,"\"}")
		if(flog==0){
			cat("Failed to generate list of tables to process.",sep ="\n",file = zz, append=TRUE)
			if(logSessionInfo){
				cat(sitd,sep="\n",file=zz,append=TRUE)
			}
			cat("\n","\n",file = zz, append=TRUE)
			cat("Terminating batch raster processing.",sep ="\n",file = zz, append=TRUE)
			close(zz)
			svlg<-try(put_object(file=logfile,object=savelog,bucket="offshore-wind-data"),silent=TRUE)
		}
		return(retJSON)
	}
	
	#subst by the species of interest
	tablesToProcess<-subset(tablesToProcess,ScientificName==sciName)
	#check that we have something to process!!!
	if(nrow(tablesToProcess)==0){
		retJSON<-"{\"Out\":[{\"Result\":\"Error\",\"Process\":\"List tables to process\",\"value\":\"List is of length 0\"}], \"Description\":\"Nothing to process.\"}"
		if(flog==0){
			cat("List of tables to process is of length 0 - nothing to process.",sep ="\n",file = zz, append=TRUE)
			if(logSessionInfo){
				cat(sitd,sep="\n",file=zz,append=TRUE)
			}
			cat("\n","\n",file = zz, append=TRUE)
			cat("Terminating batch raster processing.",sep ="\n",file = zz, append=TRUE)
			close(zz)
			svlg<-try(put_object(file=logfile,object=savelog,bucket="offshore-wind-data"),silent=TRUE)
		}
		return(retJSON)
	}
	
	## READY TO PROCESS
	## reproject and update AT
	updateResults<-data.frame()
	
	## Loop through each layer and reproject and extract to the Aliquote grid - this is what could be parallelized
	for(ii in 1:nrow(tablesToProcess)){
		#targets (generated by the function tablesToPocess
		datasetNm<-as.character(tablesToProcess[ii,"datasetName"]) 
		layerNm<-as.character(tablesToProcess[ii,"layerName"]) 
		season<-as.character(tablesToProcess[ii,"Season"]) 
		tmpRastNm=paste0(localTempDir,layerNm,"_inpRast.tif")
		tmpInpRast<-try(save_object(object=inpRast,bucket="offshore-wind-data",file=tmpRastNm),silent=T)
		rastLayer<-try(raster(tmpInpRast),silent=TRUE)
				
		if(!inherits(rastLayer,"try-error")){
			if(flog==0){
				cat(paste("Processing layer:",layerNm),sep ="\n",file = zz, append=TRUE)
			}
			origProj<-projection(rastLayer)
			orEx<-as.matrix(extent(rastLayer))
			origExt<-paste("xmin:",orEx[1,1]," xmax:",orEx[1,2]," ymin:",orEx[2,1]," ymax:",orEx[2,2])
			origExtent<-origExt
			
			sppTable<-try(projectRaster(from=rastLayer, to=aliqgrd),silent=TRUE)							
	
			## HERE crop to the EEZ - make it optional in the YAML  IFF projectRaster output is not size and extent of aliquote grid
			
			if(!inherits(sppTable,"try-error")){
				spptable<-as.data.frame(sppTable)
				spptable$gridCellId<-1:nrow(spptable)
				## Collect metadata
				dateCreated<-format(Sys.time(),"%Y-%m-%d %H:%M")
				metadf<-data.frame(SourceGeodatabase=datasetNm,
						SourceLayer=layerNm,
						SourceProjection=origProj,
						TargetProjection=projAliq,
						SourceExtent=origExtent,
						TargetExtent=targExt,
						AggregationFunction="bilinear",
						DateCreated=dateCreated
				)
				
				## Save
				localfile<-paste0(localTempDir,layerNm,".RData")
				svl<-try(save(spptable,file=localfile),silent=TRUE)
				if(inherits(svl,"try-error")){
					errmsg<-toJSON(svl)
					retJSON<-paste0("{\"Out\":[{\"Result\":\"Error\",\"Process\":\"Save object locally\",\"value\":\"Failed to save data table locally\"}], \"Description\":\"",errmsg,"\"}")
					if(flog==0){
						cat("Processed raster but failed to save data table locally.",sep ="\n",file = zz, append=TRUE)
						if(logSessionInfo){
							cat(sitd,sep="\n",file=zz,append=TRUE)
						}
						cat("\n","\n",file = zz, append=TRUE)
						cat("Terminating batch raster processing.",sep ="\n",file = zz, append=TRUE)
						close(zz)
						svlg<-try(put_object(file=logfile,object=savelog,bucket="offshore-wind-data"),silent=TRUE)
					}
					return(retJSON)
				}
				saveobject<-paste0(savePath,layerNm,".RData") 
				svb<-try(put_object(file=localfile,object=saveobject,bucket="offshore-wind-data"),silent=TRUE)
				if(inherits(svb,"try-error")){
					errmsg<-toJSON(svb)
					retJSON<-paste0("{\"Out\":[{\"Result\":\"Error\",\"Process\":\"Save object in bucket\",\"value\":\"Failed to save data table in the bucket\"}], \"Description\":\"",errmsg,"\"}")
					if(flog==0){
						cat("Processed raster but failed to save data table in the bucket.",sep ="\n",file = zz, append=TRUE)
						if(logSessionInfo){
							cat(sitd,sep="\n",file=zz,append=TRUE)
						}
						cat("\n","\n",file = zz, append=TRUE)
						cat("Terminating batch raster processing.",sep ="\n",file = zz, append=TRUE)
						close(zz)
						svlg<-try(put_object(file=logfile,object=savelog,bucket="offshore-wind-data"),silent=TRUE)
					}
					return(retJSON)
				}
				
				## Tabulate date of creation
				recId<-as.character(tablesToProcess[ii,"recId"])
				season<-as.character(tablesToProcess[ii,"Season"])
				dateVal<-format(Sys.time(),"%Y-%m-%d %H:%M")
				if(season=="Summer"){  #Can only be one of four...
					newdata<-list(SummerTableVer=dateVal)
				}else if(season=="Fall"){
					newdata<-list(FallTableVer=dateVal)
				}else if(season=="Winter"){
					newdata<-list(WinterTableVer=dateVal)
				}else{
					newdata<-list(SpringTableVer=dateVal)
				}
				
				#Update the AT to append the date
				appendtry<-try(air_update(base="appO3EgtQh3UREeun",table_name=atTable,
								record_id=recId,record_data=newdata),silent=TRUE)
				if(inherits(appendtry,"try-error")){
					tdf<-data.frame(Base=datasetNm,Layer=layerNm,Season=season,
							ReprojectionResult="Failed to update metadata on raster in AirTable",
							ATupdateResult=paste(appendtry,collapse="; "))
					if(flog==0){
						cat("Failed to update metadata on raster in AirTable",sep ="\n",file = zz, append=TRUE)
					}
				}else{
					tdf<-data.frame(Base=datasetNm,Layer=layerNm,Season=season,
							ReprojectionResult="Successfully reprojected and saved",
							ATupdateResult="Successfully updated AT record")
					if(flog==0){
						cat("Successfully reprojected and saved the raster.",sep ="\n",file = zz, append=TRUE)
					}
				}
			}else{
				tdf<-data.frame(Base=datasetNm,Layer=layerNm,Season=season, 
						ReprojectionResult=paste(sppTable, collapse="; "),
						ATupdateResult="Failed to re-project raster to aliquote grid")
				if(flog==0){
					cat("Failed to re-project raster to aliquote grid.",sep ="\n",file = zz, append=TRUE)
				}
			}
		}else{ #failed to read the raster layer
			tdf<-data.frame(Base=datasetNm,Layer=layerNm,Season=season,
					ReprojectionResult="Failed to load the raster layer",
					ATupdateResult=paste(rastLayer,collapse="; "))
			if(flog==0){
				cat("Failed to load the raster layer.",sep ="\n",file = zz, append=TRUE)
			}
		}
		updateResults<-rbind(updateResults,tdf)
	}
	retJSON<-toJSON(updateResults)	
	#retJSON<-paste0("{\"Out\":[{\"Result\":\"Success\",\"Process\":\"Everything but processing\",\"value\":\"Checks passed\"}], \"Description\":\"All good\"}")
	
	if(flog==0){
		cat("Completed processing rasters.",sep ="\n",file = zz, append=TRUE)
		if(logSessionInfo){
			cat(sitd,sep="\n",file=zz,append=TRUE)
		}
		cat("\n","\n",file = zz, append=TRUE)
		cat("Terminating batch raster processing.",sep ="\n",file = zz, append=TRUE)
		close(zz)
		svlg<-try(put_object(file=logfile,object=savelog,bucket="offshore-wind-data"),silent=TRUE)
	}
	return(retJSON)
	
}




# Author: lsalas
###############################################################################


## Output reporting function
# result is a string, either "Error" or "Success"
# process is a string naming the process evaluated
# description is either a string or a data.frame or itself a json string
makeOutReport<-function(result, process="Some process", description="some description"){
	if(class(description)=="data.frame"){
		desc<-jsonlite::toJSON(description)
	}else{
		desc<-description
	}
	tdf<-data.frame(Result=result,Process=process,Description=as.character(desc))
	return(jsonlite::toJSON(tdf))
}


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
		wdf<-subset(wdf,LoadStatus=="FALSE")
		return(makeOutReport(result="Error",process="Check libraries",description=wdf))
	}
		
	## Is the input a valid JSON string?
	if(!jsonlite::validate(inpJSON)[[1]]){
		return(makeOutReport(result="Error",process="Check input JSON",description="Invalid JSON"))
	}
	
	#We have a valid JSON
	inplst<-jsonlite::fromJSON(inpJSON)
	
	# Assert:
	#Can connect to bucket and find yaml (i.e., bucket exists)?
	inpBuck<-"offshore-wind-test"
	bktt<-aws.s3::bucket_exists(inpBuck)
	if(!bktt[1]){
		return(makeOutReport(result="Error",process="Bucket access",description="Can't access the bucket"))
	}
	
	# The yaml exists, because it triggered the event. Can we read it? Is the yaml valid?
	inpYAML<-inplst$Records$s3$object$key[[1]][1]  #This should be the full path to the yaml, right???
	if(nchar(inpYAML)<20){
		return(makeOutReport(result="Error",process="JSON key",description="Invalid path to yaml file"))
	}
	
	#  Can we load it? Is the input a valid yaml document?
	chryaml<-rawToChar(get_object(bucket=inpBuck,object=inpYAML))
	ymlc<-try(yaml::yaml.load(chryaml),silent=TRUE)
	if(inherits(ymlc,"try-error")){
		errmsg<-jsonlite::toJSON(as.character(ymlc))
		return(makeOutReport(result="Error",process="Read yaml",description=errmsg))
	}
	
	# Validate yaml contents - must have:
	# AT key and db key
	atKF<-ymlc$ATkeyfile
	chkKF<-suppressMessages(object_exists(bucket=inpBuck,object=paste0("FirstProcess/keys/",atKF)))
	# Target AT table
	atTable<-ymlc$ATtable
	chkTable<-atTable %in% c("Seabirds","MarineMammalsTurtles","Fish","Benthic","HumanUses")
	# Target species
	sciName<-ymlc$sciName
	chkSpp<-ifelse(sciName=="",FALSE,TRUE)
	# season (default all)
	chkSeasons<-toupper(ymlc$seasons) %in% c("",NULL,NA,"ALL","SUMMER","FALL","WINTER","SPRING")
	allseasons<-c("Summer","Fall","Winter","Spring")
	seasons<-ifelse(ymlc$seasons=="",allseasons,
			ifelse(is.na(ymlc$seasons),allseasons,
					ifelse(is.null(ymlc$seasons),allseasons,
							ifelse(toupper(ymlc$seasons)=="ALL",allseasons,ymlc$seasons))))
		
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
	if(!chkKF | !chkTable | !chkSpp | !chkRast | !chkGrid | !chkSeasons | !chkSavePath | !chkLogDir){
		errmsg<-paste("One or more of the following are wrong or missing in the input yaml: AirTable key file name, target table or species,",
				"input raster or base grid filepath, a valid season, output directory path, log directory path.")
		return(makeOutReport(result="Error",process="Validate yaml",description=errmsg))
	}
	
	## Load the grid - this is the reference grid to rasterize to - path in YAML
	# Check the local temp directory:
	localTempDir<-tempdir()
	ltdchk<-dir.exists(localTempDir)
	if(!ltdchk){
		return(makeOutReport(result="Error",process="Check local temp directory",description="Can't find local temp directory"))
	}
	
	#create log file for this run:
	filelogn<-paste0("OSW_CA_batchRasterProcessing_",logstart)
	logfile<-paste(localTempDir,filelogn,".log",sep="")
	savelog<-paste0(logDir,filelogn,".log")
	sitd<-as.character(sessionInfo())
	
	zz <- try(file(logfile, "w"),silent=T)
	flog<-0
	if(inherits(zz,"try-error")){
		cat(makeOutReport(result="Error",process="Create log file",description="Failed to create log file in local temp directory. No log of this run."))
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
	wgrd<-try(save_object(object="FirstProcess/AliquoteGrid/aliqgrid.grd",bucket=inpBuck,file=paste0(localTempDir,"aliqgrid.grd")),silent=T)
	wgri<-try(save_object(object="FirstProcess/AliquoteGrid/aliqgrid.gri",bucket=inpBuck,file=paste0(localTempDir,"aliqgrid.gri")),silent=T)
	if(inherits(wgrd,"try-error") | inherits(wgri,"try-error")){
		if(flog==0){
			cat("Failed to copy base grid locally.",sep ="\n",file = zz, append=TRUE)
			if(logSessionInfo){
				cat(sitd,sep="\n",file=zz,append=TRUE)
			}
			cat("\n","\n",file = zz, append=TRUE)
			cat("Terminating batch raster processing.",sep ="\n",file = zz, append=TRUE)
			close(zz)
			svlg<-try(put_object(file=logfile,object=savelog,bucket=inpBuck),silent=TRUE)
		}
		return(makeOutReport(result="Error",process="Copy base grid",description="Failed to copy base grid into the local temp directory"))
	}
	fegrd<-file.exists(paste0(localTempDir,"aliqgrid.grd"))
	fegri<-file.exists(paste0(localTempDir,"aliqgrid.grd"))
	if(!fegrd | !fegri){
		if(flog==0){
			cat("Failed to copy base grid locally.",sep ="\n",file = zz, append=TRUE)
			if(logSessionInfo){
				cat(sitd,sep="\n",file=zz,append=TRUE)
			}
			cat("\n","\n",file = zz, append=TRUE)
			cat("Terminating batch raster processing.",sep ="\n",file = zz, append=TRUE)
			close(zz)
			svlg<-try(put_object(file=logfile,object=savelog,bucket=inpBuck),silent=TRUE)
		}
		return(makeOutReport(result="Error",process="Copy base grid",description="Failed to copy base grid into the local temp directory"))
	}

	aliqgrd<-try(raster(paste0(localTempDir,"aliqgrid.gri")),silent=T)
	if(inherits(aliqgrd,"try-error")){
		if(flog==0){
			cat("Failed to load the base grid into a raster.",sep ="\n",file = zz, append=TRUE)
			if(logSessionInfo){
				cat(sitd,sep="\n",file=zz,append=TRUE)
			}
			cat("\n","\n",file = zz, append=TRUE)
			cat("Terminating batch raster processing.",sep ="\n",file = zz, append=TRUE)
			close(zz)
			svlg<-try(put_object(file=logfile,object=savelog,bucket=inpBuck),silent=TRUE)
		}
		return(makeOutReport(result="Error",process="Load base grid",description="Base grid exists and is acessible, but failed to load."))
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
	atKobj<-rawToChar(get_object(bucket=inpBuck,object=paste0("FirstProcess/keys/",atKF)))
	ymlatk<-try(yaml::yaml.load(atKobj),silent=TRUE)
	if(inherits(ymlatk,"try-error")){
		errmsg<-jsonlite::toJSON(as.character(ymlatk))
		return(makeOutReport(result="Error",process="Read AT keys from yaml file",description=errmsg))
	}
	
	Sys.setenv(AIRTABLE_API_KEY=ymlatk$Key)
	tablesToProcess<-try(getTablesToProcess(atSourceTable=atTable,seasons=seasons),silent=TRUE) 
	if(inherits(tablesToProcess,"try-error") | nrow(tablesToProcess)==0){
		if(flog==0){
			cat("Failed to generate list of tables to process.",sep ="\n",file = zz, append=TRUE)
			if(logSessionInfo){
				cat(sitd,sep="\n",file=zz,append=TRUE)
			}
			cat("\n","\n",file = zz, append=TRUE)
			cat("Terminating batch raster processing.",sep ="\n",file = zz, append=TRUE)
			close(zz)
			svlg<-try(put_object(file=logfile,object=savelog,bucket=inpBuck),silent=TRUE)
		}
		errmsg<-jsonlite::toJSON(as.character(tablesToProcess))
		return(makeOutReport(result="Error",process="List tables to process",description=errmsg))
	}
	
	#subst by the species of interest
	tablesToProcess<-subset(tablesToProcess,ScientificName==sciName)
	#check that we have something to process!!!
	if(nrow(tablesToProcess)==0){
		if(flog==0){
			cat("List of tables to process is of length 0 - nothing to process.",sep ="\n",file = zz, append=TRUE)
			if(logSessionInfo){
				cat(sitd,sep="\n",file=zz,append=TRUE)
			}
			cat("\n","\n",file = zz, append=TRUE)
			cat("Terminating batch raster processing.",sep ="\n",file = zz, append=TRUE)
			close(zz)
			svlg<-try(put_object(file=logfile,object=savelog,bucket=inpBuck),silent=TRUE)
		}
		return(makeOutReport(result="Error",process="List tables to process",description="List is of length 0, nothing to process."))
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
		tmpInpRast<-try(save_object(object=inpRast,bucket=inpBuck,file=tmpRastNm),silent=T)
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
					if(flog==0){
						cat("Processed raster but failed to save data table locally.",sep ="\n",file = zz, append=TRUE)
						if(logSessionInfo){
							cat(sitd,sep="\n",file=zz,append=TRUE)
						}
						cat("\n","\n",file = zz, append=TRUE)
						cat("Terminating batch raster processing.",sep ="\n",file = zz, append=TRUE)
						close(zz)
						svlg<-try(put_object(file=logfile,object=savelog,bucket=inpBuck),silent=TRUE)
					}
					errmsg<-jsonlite::toJSON(as.character(svl))
					return(makeOutReport(result="Error",process="Save object locally",description=errmsg))
				}
				saveobject<-paste0(savePath,layerNm,".RData") 
				svb<-try(put_object(file=localfile,object=saveobject,bucket=inpBuck),silent=TRUE)
				if(inherits(svb,"try-error")){
					if(flog==0){
						cat("Processed raster but failed to save data table in the bucket.",sep ="\n",file = zz, append=TRUE)
						if(logSessionInfo){
							cat(sitd,sep="\n",file=zz,append=TRUE)
						}
						cat("\n","\n",file = zz, append=TRUE)
						cat("Terminating batch raster processing.",sep ="\n",file = zz, append=TRUE)
						close(zz)
						svlg<-try(put_object(file=logfile,object=savelog,bucket=inpBuck),silent=TRUE)
					}
					errmsg<-jsonlite::toJSON(as.character(svb))
					return(makeOutReport(result="Error",process="Save object in bucket",description=errmsg))
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
				appendtry<-try(air_update(base=ymlatk$Base,table_name=atTable,
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
	
	if(flog==0){
		cat("Completed processing rasters.",sep ="\n",file = zz, append=TRUE)
		if(logSessionInfo){
			cat(sitd,sep="\n",file=zz,append=TRUE)
		}
		cat("\n","\n",file = zz, append=TRUE)
		cat("Terminating batch raster processing.",sep ="\n",file = zz, append=TRUE)
		close(zz)
		svlg<-try(put_object(file=logfile,object=savelog,bucket=inpBuck),silent=TRUE)
	}
	return(makeOutReport(result="Completed",process="Cropping and saving",description=updateResults))
	
}

# wrapper function with a common handler name makes the code more portable
main<-function(inpJSON){
    return(batchImportRasters_toOSW(inpJSON))
}


#!/bin/bash

currentDIR=`pwd`
   codeDIR=${currentDIR}/code
    pkgDIR=../301-pkg-fpcFeatures/code
 outputDIR=${currentDIR//github/gittmp}/output

parentDIR=`dirname ${currentDIR}`
  dataDIR=${parentDIR}/000-data

if [ ! -d ${outputDIR} ]; then
	mkdir -p ${outputDIR}
fi

cp -r ${codeDIR} ${outputDIR}
cp -r  ${pkgDIR} ${outputDIR}
cp    $0         ${outputDIR}/code

##################################################
myRscript=${codeDIR}/main-01.R
stdoutFile=${outputDIR}/stdout.R.`basename ${myRscript} .R`
stderrFile=${outputDIR}/stderr.R.`basename ${myRscript} .R`
R --no-save --args ${dataDIR} ${codeDIR} ${pkgDIR} ${outputDIR} < ${myRscript} > ${stdoutFile} 2> ${stderrFile}
sleep 30

### ~~~~~~~~~~ ###
if [ `uname` != Darwin ]
then
    ### ~~~~~~~~~~ ###
    myRscript=${codeDIR}/main-02.R
    stdoutFile=${outputDIR}/stdout.R.`basename ${myRscript} .R`
    stderrFile=${outputDIR}/stderr.R.`basename ${myRscript} .R`
    R --no-save --args ${dataDIR} ${codeDIR} ${pkgDIR} ${outputDIR} < ${myRscript} > ${stdoutFile} 2> ${stderrFile}
    sleep 30
    ### ~~~~~~~~~~ ###
    for variable in VH VV
    do
        for year in 2017 2018 2019
        do
            myRscript=${codeDIR}/main-03.R
            stdoutFile=${outputDIR}/stdout.R.`basename ${myRscript} .R`-${variable}-${year}
            stderrFile=${outputDIR}/stderr.R.`basename ${myRscript} .R`-${variable}-${year}
            R --no-save --args ${dataDIR} ${codeDIR} ${pkgDIR} ${outputDIR} ${variable} ${year} < ${myRscript} > ${stdoutFile} 2> ${stderrFile}
            sleep 30
        done
    done
    sleep 30
    ### ~~~~~~~~~~ ###
    cd ${outputDIR}
    Rscript -e 'csv.directory <- "data-unlabelled-fpc-scores-csv"; if (!dir.exists(csv.directory)) { dir.create(path = csv.directory, recursive = TRUE) } temp.files <- list.files(path=".",pattern="batch.+RData$"); for (temp.file in temp.files) { print(temp.file); temp.stem <- tools::file_path_sans_ext(temp.file); DF.temp <- readRDS(temp.file); print(str(DF.temp)); write.csv(x = DF.temp, file = file.path(csv.directory,paste0(temp.stem,".csv")), row.names = FALSE) }'
fi

##################################################
exit 0


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
# R --no-save --args ${dataDIR} ${codeDIR} ${pkgDIR} ${outputDIR} < ${myRscript} > ${stdoutFile} 2> ${stderrFile}

### ~~~~~~~~~~ ###
for variable in VH VV
do
    for year in 2017 2018 2019
    do
        myRscript=${codeDIR}/main-02.R
        stdoutFile=${outputDIR}/stdout.R.`basename ${myRscript} .R`-${variable}-${year}
        stderrFile=${outputDIR}/stderr.R.`basename ${myRscript} .R`-${variable}-${year}
        echo "${variable} ${year} ${stdoutFile} ${stderrFile}"
        R --no-save --args ${dataDIR} ${codeDIR} ${pkgDIR} ${outputDIR} ${variable} ${year} < ${myRscript} > ${stdoutFile} 2> ${stderrFile} &
    done
done

##################################################
exit 0


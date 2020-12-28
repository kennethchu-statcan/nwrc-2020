#!/bin/bash

currentDIR=`pwd`
   codeDIR=${currentDIR}/code
 outputDIR=${currentDIR//github/gittmp}/output

if [ ! -d ${outputDIR} ]; then
    mkdir -p ${outputDIR}
fi

cp -r ${codeDIR} ${outputDIR}
cp    $0         ${outputDIR}/code

########################################################
##### Assemble contents of R package
packageName=fpcFeatures

myRscript=${codeDIR}/main-assemble-package.R
stdoutFile=${outputDIR}/stdout.R.`basename ${myRscript} .R`
stderrFile=${outputDIR}/stderr.R.`basename ${myRscript} .R`
Rscript ${myRscript} ${codeDIR} ${outputDIR} ${packageName} > ${stdoutFile} 2> ${stderrFile}

##### Check the newly built R package
# remember: define the environment variable R_LIBS_USER
# so this build script knows where the dependencies are,
# so that the following command can work properly.

cd ${outputDIR}/build-no-vignettes
R CMD check ${packageName}_*.tar.gz > stdout.R.check 2> stderr.R.check
# R CMD check --as-cran ${packageName}_*.tar.gz > ${outputDIR}/stdout.R.check 2> ${outputDIR}/stderr.R.check

cd ${outputDIR}/build-vignettes
R CMD check ${packageName}_*.tar.gz > stdout.R.check 2> stderr.R.check
# R CMD check --as-cran ${packageName}_*.tar.gz > ${outputDIR}/stdout.R.check 2> ${outputDIR}/stderr.R.check

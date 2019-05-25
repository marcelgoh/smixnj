# Makefile for SMIX/NJ

SML=sml
BUILD=ml-build
BUILDDIR=_build
CM=sources.cm

$(shell mkdir -p ${BUILDDIR})

all:
	${BUILD} ${CM} Mix.mix

	sed -e "s#%MIXDIR#"`pwd`"#g" \
	    -e "s#%SML#${SML}#g" sml_load.sh \
	> mix;
	chmod a+x mix

.PHONY: clean
clean:
	rm -rf ${BUILDDIR} .cm src/.cm mix

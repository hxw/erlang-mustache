# BSDmakefile
# just a quick redirect to gmake

GMAKE = gmake
ERL_PATH = /usr/local/lib/erlang16/bin

# for no targets on command line
.MAIN: all

# do each target separately
${.TARGETS}:
	@env PATH="${PATH}:${ERL_PATH}" MFLAGS= MAKEFLAGS= ${GMAKE} ${MAKEFLAGS} ${.TARGET}

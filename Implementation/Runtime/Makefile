IMPLEMENTATION=${SCALA_LE}/Implementation
SLEC=slec
SLE_SRCS= ${wildcard sle/sle/*.scala}
CONTAINERS_SRCS = ${wildcard sle/sle/containers/*.scala}
SRCS = ${SLE_SRCS} ${CONTAINERS_SRCS}

all :: ${SRCS} classes
	${SLEC} -d classes ${SRCS}

classes :
	mkdir $@

clean :
	rm -Rf classes



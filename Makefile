install: 
	R --slave -e 'install.packages(".", repos = NULL, type  ="source")'

document:
	R --slave -e 'devtools::document()'


all: document install

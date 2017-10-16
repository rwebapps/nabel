FROM opencpu/base

RUN R -e 'devtools::install_github("rwebapps/nabel")'

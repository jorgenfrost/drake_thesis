source("_drake.R")

drake::make(the_plan)

system("cd doc/;sh ./compile_latex2.sh")

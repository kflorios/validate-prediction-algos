# validate-prediction-algos


To run the python-R code you will need:

1. A working installation of >R3.0

2. A working installation of >Python 2.7

3. package 'rpy2' in python installed

4. packages 'tseries','nnet','xtable','pracma','matrixStats','e1071','ROCR','verification' in R installed


Run has been checked in a windows 64 machine.


Guideline

Put

a) Script_Monte_YK_MX.py  (python main)
b) functions_Monte_YK1_MX.R (R functions)
c) data_neural_MX.txt (input file)

in same directory and change line 64 in python main with root = current working directory

Run Script_Monte_YK_MX.py in Spyder
The script takes 1h to run.

If you change lines 176-182 in python main, such that 
fname="data_neural_MX.txt"
mspace=range(1,6)

it should take less than five minutes to run.

In the current directory many files are produced.

The final result is the skill score statistics ACC, TSS, HSS for all methods which is also produced as a latex file,
and displayed at the console tab window of Spyder with latex code.

Also, figures myfig0, myfig1, myfig2, myfig3, myfig5 (see myfig4 is skipped)
are produced for all five methods validated.

The methods are
0: Neural network
1: Linear regression
2: Probit regression
3: Logit regression
4: skipped
5: Support vector Machine


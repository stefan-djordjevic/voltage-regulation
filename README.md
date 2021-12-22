# voltage-regulation

VOLTAGE REGULATION IN DISTRIBUTION NETWORK WITH DISTRIBUTED GENERATOR

Given tasks are done in FORTRAN and C++.

IDE Microsoft Visual Studio 2010 is used to develop this project. Solution consists of the following:

C++_DLL_Project – as Dynamic-Link Library (.dll)

FORTRAN_Project – as Console Application (main project; depends on C++_DLL_Project;)

The project was completed with the documentation (pdf file) covering theoretical basics on which the task in the project is based. 
The documentation contains an attachment with the input data presented in external files, also. The implementation of the program itself involves the implementation of most a feature of Fortran and C++ programming languages.

THE TASK IN SHORT:
Apply a voltage regulation system to the test distribution network (Figure 2.1) to maintain the best possible voltage across consumers. Use an automatic voltage regulator (ARN) with control transformer (RTr) as a voltage regulation resource. In addition, examine the effects of connecting a distributed generator to the distribution network, on the quality of voltage regulation performed by an ARN with a preset control law. Show results and comment.

EXTERNAL FILES with the input data (PRuEE_Projekat\FORTRAN_PROJEKAT\ULAZ_DATOTEKE\...):

ULAZ_Bazne_Vrednosti.txt; 
ULAZ_Broj_Elemenata.txt; 
ULAZ_Deonice_Podaci.txt; 
ULAZ_Potr_Podaci.txt; 
ULAZ_Topol_Podaci.txt; 
ULAZ_Transformatori_Podaci.txt; 

EXTERNAL FILES with the output data – results (PRuEE_Projekat\FORTRAN_PROJEKAT\IZLAZ_DATOTEKE\ ...):

IZLAZ_KONFIGURACIJA MREZE.txt; 
IZLAZ_REG_Isn; 
IZLAZ_REG_Uost.sn; 
IZLAZ_REGULACIJA(sa stetom); 
IZLAZ_REGULACIJA; 
Kontrolni_Fajl_Tokovi_Snaga; 
Kontrolni_Fajl_Zakon_Regulacije; 


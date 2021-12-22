#define PROJECT_DLLEXPORT __declspec(dllexport)

#include"Complex.h"
#include <cmath>

//-------------------------------------------------------------------------------------------------------------
//Relacija za proracun injektiranih struja cvorova (relacija 3.4 iz dokumentacije)

extern "C" 
{
	void PROJECT_DLLEXPORT Proracun_I_inj_Cvora ( Complex& Sp, Complex& Y0, Complex& Vc, Complex& I_in )
	{
		I_in = ~(Sp / Vc) + Y0 * Vc ;
	}
}


//-------------------------------------------------------------------------------------------------------------
//Relacija za proracun struja po granama (zamena unazad - pocevsi od grana u poslednjem lejeru, relacija 3.7 iz dokumentacije)

extern "C" 
{
	void PROJECT_DLLEXPORT Proracun_J_Grane ( Complex& Jg_sum, Complex& Jg )
	{
		Jg_sum = Jg_sum + Jg ;
	}
}


//-------------------------------------------------------------------------------------------------------------
//Relacija za proracun napona u cvorovima (zamena unapred - pocevsi od cvora u prvom lejeru, relacija 3.8 iz dokumentacije)

extern "C" 
{	
	void PROJECT_DLLEXPORT Proracun_V_Cvora (Complex& Vc_gornji, Complex& Zg, Complex& Jg, Complex& Vc_donji) 
	{
		Vc_donji = Vc_gornji - Zg * Jg ;
	}
}


//-------------------------------------------------------------------------------------------------------------
//Relacija za proracun injektirane snage cvorova (relacija 3.15 iz dokumentacije)

extern "C" 
{	
	void PROJECT_DLLEXPORT Proracun_S_Cvora (Complex& Vc, Complex& I_in, Complex& Y0, Complex& U, Complex& S) 
	{
		S = Vc * (~(I_in)) - (~(Y0)) * abs(U) * abs(U);
	}
}
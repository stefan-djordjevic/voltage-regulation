#include "Complex.h" 
#include <math.h>
	
// Podrazumevani konstruktor
	Complex::Complex(){                                
		real = 0.00;
		imag = 0.00; }

// Konstruktor sa parametrima
	Complex::Complex( float Re, float Im){             
		real = Re;
		imag = Im; }
 
// Konstruktor kopije
	Complex::Complex( const Complex &br) {             
		real = br.real;
		imag = br.imag; }

// Get metoda za realni deo kompleksnog broja
	float  Complex::getReal() const {                  
		return real;  }

// Get metoda za imaginarni deo kompleksnog broja
	float  Complex::getImag() const {                 
		return imag;  }

// Set metoda za realni deo kompleksnog broja
	void  Complex::setReal (const float Re) {         
		real = Re;  }

 // Set metoda za imaginarni deo kompleksnog broja
	void  Complex::setImag (const float Im) {         
		imag = Im;  }

// Operator dodele
	Complex &Complex::operator= (const Complex &z) {  
		real = z.real; 
		imag = z.imag; 
		return *this; }

// Osnovne matematicke operacije sa kompleksnim brojevima:
	Complex operator+ (const Complex &z1, const Complex &z2) { 
		Complex temp(z1.real + z2.real, z1.imag + z2.imag);
		return temp;  } 
 
	Complex operator- (const Complex &z1, const Complex &z2) {
		Complex temp(z1.real - z2.real, z1.imag - z2.imag);
		return temp;  } 
 
	Complex operator* (const Complex &z1, const Complex &z2) {
		Complex temp(z1.real*z2.real - z1.imag*z2.imag, z1.imag*z2.real + z1.real*z2.imag);
		return temp;  } 

	Complex operator* (const Complex &z, const float d) {
		Complex temp(z.real * d, z.imag * d);
		return temp;  }

	Complex operator/ (const Complex &z1, const Complex &z2) {
		float  d = z2.real*z2.real + z2.imag*z2.imag; 
		Complex temp((z1.real*z2.real + z1.imag*z2.imag) / d, (z1.imag*z2.real - z1.real*z2.imag) / d);
		return temp;  } 

	Complex operator/ (const int & ceoBroj, const Complex &z2) {
		Complex z1((float)ceoBroj,(float)0);

		float  d = z2.real*z2.real + z2.imag*z2.imag; 
		Complex temp((z1.real*z2.real + z1.imag*z2.imag) / d, (z1.imag*z2.real - z1.real*z2.imag) / d);
		return temp;  }

	Complex operator~ (const Complex &z) { 
        Complex conj;
		conj.real = z.real;
        conj.imag = -z.imag;
		return conj; }

	float abs (const Complex &z) {
		float module = sqrt(z.real*z.real + z.imag*z.imag);
		return module;  }
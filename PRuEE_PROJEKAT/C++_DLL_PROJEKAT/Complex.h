#ifndef COMPLEX_DEFF 
#define COMPLEX_DEFF

#include <iostream> 
using namespace std; 
 
class Complex
{ 
 private: 

    float  real;	// realni deo kompleksnog broja
	float  imag;	// imaginarni deo kompleksnog broja
 
 public: 

	Complex();                  // Podrazumevani konstruktor
    Complex(float, float);      // Konstruktor sa parametrima
    Complex(const Complex &);   // Konstruktor kopije
	~Complex(){}                // Destruktor

    float  getReal() const;     // Get metoda za realni deo kompleksnog broja
    float  getImag() const;     // Get metoda za imaginarni deo kompleksnog broja

    void setReal( const float); // Set metoda za realni deo kompleksnog broja
    void setImag( const float); // Set metoda za imaginarni deo kompleksnog broja
	
    Complex &operator=( const Complex &);                         // Operator dodele

	// Osnovne matematicke operacije sa kompleksnim brojevima:
    friend Complex operator+( const Complex &, const Complex &);  
	friend Complex operator+( const int &, const Complex &); 
    friend Complex operator-( const Complex &, const Complex &); 
    friend Complex operator*( const Complex &, const Complex &); 
	friend Complex operator*( const Complex &, const float );
    friend Complex operator/( const Complex &, const Complex &);
	friend Complex operator/( const int &, const Complex &);
	friend Complex operator~( const Complex &);

	friend float abs( const Complex &);
}; 
 
#endif 
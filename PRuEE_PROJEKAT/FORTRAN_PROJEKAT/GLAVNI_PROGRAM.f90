PROGRAM Glavni


USE EES_Elementi, ONLY: ID_File
USE EES_Ucitavanje
USE Topologija_Helper
USE Bazne_Velicine
USE Regulacija
!USE Tokovi_Snaga !@@@@@@@@@@@@@@@@@@@@@@@@


IMPLICIT NONE


INTERFACE


    SUBROUTINE Ispis(jed_Ispisa)

        INTEGER, INTENT(IN) :: jed_Ispisa

    END SUBROUTINE Ispis


END INTERFACE


WRITE (*, *) 'PROGRAM PRORACUN TOKOVA SNAGA SA REGULACIJOM NAPONA JE POKRENUT...'

CALL SLEEP(1)


!....................................................................................................
!Ucitavanje podataka o broju elemanata EES

Global_Error = Ucitaj_Broj_Elemenata_Mreze()
IF (Global_Error) STOP ("[ERROR] Problem prilikom ucitavanja podataka o broju elemenata mreze!")


!....................................................................................................
!Ucitavanje podataka za VODOVE iz ulazne datoteke

Global_Error = Ucitaj_Vodove()
IF (Global_Error) STOP ("[ERROR] Problem prilikom ucitavanja podataka za vodove!")


!....................................................................................................
!Ucitavanje podataka za POTROSACE iz ulazne datoteke

Global_Error = Ucitaj_Potrosace()
IF (Global_Error) STOP ("[ERROR] Problem prilikom ucitavanja podataka za potrosace!")

!....................................................................................................
!Ucitavanje podataka za POTROSACE iz ulazne datoteke

Global_Error = Ucitaj_Transformatore()
IF (Global_Error) STOP ("[ERROR] Problem prilikom ucitavanja podataka za transformatore!")


!....................................................................................................
!Ucitavanje podataka za topologiju iz ulazne datoteke 'Topol_Podaci'

CALL Ucitaj_Topologiju('ULAZ_DATOTEKE\ULAZ_Topol_Podaci.txt')


!....................................................................................................
!Proracun parametara cvorova

CALL Proracun_P0_Q0_za_CVOR()


!....................................................................................................
!Proracun parametara grana

CALL Proracun_Parametara_GRANA()


!....................................................................................................
!Ucitavanje podataka za bazne velicine iz ulazne datoteke

Global_Error = Ucitaj_Bazne_Velicine()
IF (Global_Error) STOP ("[ERROR] Problem prilikom ucitavanja podataka za bazne velicine!")


!....................................................................................................
!Proracun izvedenih baznih velicina

CALL Proracun_Izvedenih_Baznih_Velicina()


!....................................................................................................
!Pretvaranje velicina u relativne jedinice 

CALL Pretvarac_APS_REL(APS_u_REL)


!....................................................................................................
!Ispis konfiguracije mreze u izlaznu datoteku

CALL Ispis_Konfiguracije_Mreze(ID_File)


!....................................................................................................
!Regulacija Tr-a na optimalne pozicije i ispis rezultata u izlaznu datoteku

CALL REGULACIJA_NAPONA()

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!CALL Proracun_Tokova_Snaga(1, 0, 1.)
!CALL Ispis_Konfiguracije_Mreze(ID_File)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!....................................................................................................
!Oslobadjanje dinamicke memorije

CALL Dealokacija()


WRITE (*, '(a, /)') 'PROGRAM USPESNO IZVRSEN.'


PAUSE
END PROGRAM Glavni
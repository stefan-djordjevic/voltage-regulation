MODULE EES_Ucitavanje

USE EES_Elementi


IMPLICIT NONE


INTEGER :: Alloc_Error

LOGICAL:: Global_Error

INTEGER :: i

!********************************************************************************************
!Promenljive vezane za elemente mreze

CLASS(EES_Podaci), POINTER :: p_EES      => NULL()    

CLASS(Element_EES), POINTER :: p_Element => NULL()

CLASS(VOD), POINTER :: p_Vod             => NULL()

CLASS(POTROSAC), POINTER :: p_Potrosac   => NULL()

CLASS(TRANSFORMATOR), POINTER :: p_Transformator   => NULL()

!********************************************************************************************
!Deklaracija dinamickih nizova

TYPE(VOD), ALLOCATABLE, TARGET :: vodovi(:)

TYPE(POTROSAC), ALLOCATABLE, TARGET :: potrosaci(:)

TYPE(TRANSFORMATOR), ALLOCATABLE, TARGET :: transformatori(:)

!********************************************************************************************



CONTAINS

    
    !********************************************************************************************
    !Ucitavanje podataka o broju elemanata EES

    FUNCTION Ucitaj_Broj_Elemenata_Mreze() RESULT(Error)


        LOGICAL :: Error

        Error = .false.


        WRITE (*, *) '  Ucitavanje podataka o broju elemanata EES...'
        
        CALL SLEEP(1)    !Stopirano izvrsavanje na 1 sekund

        !Kreiranje objekta tipa EES_Podaci
        ALLOCATE(p_EES, STAT = Alloc_Error)

        IF (Alloc_Error .NE. 0) THEN
            Error = .true.
            STOP ("[ERROR] Problem sa alokacijom p_EES!")
        END IF

        !Ucitavanaje podataka
        Global_Error = p_EES%Ucitaj_EES()


    END FUNCTION Ucitaj_Broj_Elemenata_Mreze

    !********************************************************************************************
    !Ucitavanje podataka za VODOVE iz ulazne datoteke

    FUNCTION Ucitaj_Vodove() RESULT(Error)
        

        LOGICAL :: Error

        Error = .false.


        WRITE (*, *) '  Ucitavanje podataka za vodove...'

        CALL SLEEP(1)

        !............................................................................................
        !Alokacija memorije za vodove

        IF ( .NOT.ALLOCATED(vodovi) ) ALLOCATE ( vodovi(p_EES%Get_Broj_Vodova()), STAT = Alloc_Error )
        IF (Alloc_Error .NE. 0) THEN
            Error = .true.
            STOP ("[ERROR] Problem sa alokacijom memorije za vodove!")
        END IF

        !............................................................................................
        
        
        OPEN (ID_File, FILE='ULAZ_DATOTEKE\ULAZ_Deonice_Podaci.txt', ACTION = 'READ', STATUS = 'OLD', ERR = 100)

       
        UCITAVANJE_VODOVA: DO i = 1, p_EES%Get_Broj_Vodova()
            p_Element => vodovi(i)
            Global_Error = p_Element%Ucitaj()    !Metoda definisana u okviru natklase i preklopljena u
        ENDDO UCITAVANJE_VODOVA                  !okviru potklase -> princip polimorfizma automatski "radi"


        CLOSE(ID_File)


        RETURN

        100 WRITE (*,*)  "[ERROR] Ne postoji ulazna datoteka 'Deonice_Podaci' na odgovarajucoj lokaciji!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_Vodove

    !********************************************************************************************
    !Ucitavanje podataka za POTROSACE iz ulazne datoteke

    FUNCTION Ucitaj_Potrosace() RESULT(Error)


        LOGICAL :: Error

        Error = .false.


        WRITE (*, *) '  Ucitavanje podataka za potrosace...'

        CALL SLEEP(1)

        !............................................................................................
        !Alokacija memorije za potrosace

        IF ( .NOT.ALLOCATED(potrosaci) ) ALLOCATE ( potrosaci(0 : p_EES%Get_Broj_Potrosaca()), STAT = Alloc_Error )
        IF (Alloc_Error /= 0) THEN
            Error = .true.
            STOP ("[ERROR] Problem sa alokacijom memorije za potrosace!")
        END IF

        !............................................................................................


        OPEN (ID_File, FILE='ULAZ_DATOTEKE\ULAZ_Potr_Podaci.txt', ACTION = 'READ', STATUS = 'UNKNOWN', ERR = 100)

       
        UCITAVANJE_POTROSACA: DO i = 0, p_EES%Get_Broj_Potrosaca()
            p_Element => potrosaci(i)
            Global_Error = p_Element%Ucitaj()
        ENDDO UCITAVANJE_POTROSACA


        CLOSE(ID_File)


        RETURN

        100 WRITE (*,*)  "[ERROR] Ne postoji ulazna datoteka 'Potr_Podaci' na odgovarajucoj lokaciji!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_Potrosace

    !********************************************************************************************
    !Ucitavanje podataka za TRANSFORMATORE iz ulazne datoteke

    FUNCTION Ucitaj_Transformatore() RESULT(Error)
        

        LOGICAL :: Error

        Error = .false.


        WRITE (*, *) '  Ucitavanje podataka za transformatore...'

        CALL SLEEP(1)

        !............................................................................................
        !Alokacija memorije za transformatore

        IF ( .NOT.ALLOCATED(transformatori) ) ALLOCATE ( transformatori(p_EES%Get_Broj_Transformatora()), STAT = Alloc_Error )
        IF (Alloc_Error .NE. 0) THEN
            Error = .true.
            STOP ("[ERROR] Problem sa alokacijom memorije za transformatore!")
        END IF

        !............................................................................................
        
        
        OPEN (ID_File, FILE='ULAZ_DATOTEKE\ULAZ_Transformatori_Podaci.txt', ACTION = 'READ', STATUS = 'OLD', ERR = 100)

       
        DO i = 1, p_EES%Get_Broj_Transformatora()
            p_Element => transformatori(i)
            Global_Error = p_Element%Ucitaj()    
        ENDDO                  


        CLOSE(ID_File)


        RETURN

        100 WRITE (*,*)  "[ERROR] Ne postoji ulazna datoteka 'Transformatori' na odgovarajucoj lokaciji!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_Transformatore

    !********************************************************************************************



END MODULE EES_Ucitavanje
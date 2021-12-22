MODULE Topologija_Helper


USE EES_Elementi, ONLY: jed_Datoteke => ID_File
USE EES_Ucitavanje, ONLY: Alloc_Error, Global_Error, k => i
USE Topologija


IMPLICIT NONE


INTERFACE Inicijalizacija 
    
    MODULE PROCEDURE INT_Inicijalizacija
    MODULE PROCEDURE REAL_Inicijalizacija

END INTERFACE Inicijalizacija


!********************************************************************************************
!Promenljive vezane za topologiju mreze

INTEGER :: broj_Cvorova
INTEGER :: broj_Grana

CLASS(CVOR), POINTER:: p_Cvor => NULL()

TYPE(CVOR), ALLOCATABLE, TARGET:: cvorovi(:)

CLASS(GRANA), POINTER:: p_Grana => NULL()

TYPE(GRANA), ALLOCATABLE, TARGET:: grane(:)

!================================================================================================
!================================================================================================

CONTAINS
 
    !********************************************************************************************
    !Ucitavanje podataka za topologiju iz ulazne datoteke

    SUBROUTINE Ucitaj_Topologiju (ulazna_Datoteka)
    
    
        CHARACTER(*) :: ulazna_Datoteka
        INTEGER :: i


        WRITE (*, *) '  Ucitavanje podataka za topologiju...'

        CALL SLEEP(1)
        

        OPEN (UNIT = jed_Datoteke, FILE = ulazna_Datoteka, ACTION = 'READ', STATUS = 'UNKNOWN', ERR = 100)

        
        !............................................................................................
        !Ucitavanje podataka za cvorove
        
        READ (jed_Datoteke, '(8x, I6)', ERR=101) broj_Cvorova

   
        DO i=1, 3
            READ(jed_Datoteke, *)
        ENDDO
        
                                                                !Alokacija memorije za cvorove
        IF ( .NOT.ALLOCATED(cvorovi) ) ALLOCATE ( cvorovi(0:broj_Cvorova), STAT = Alloc_Error )
        IF (Alloc_Error .NE. 0) THEN
            STOP ("[ERROR] Problem sa alokacijom memorije za cvorove!")
        END IF


        DO i = 0, broj_Cvorova
            p_Cvor => cvorovi(i)
            Global_Error = p_Cvor%Ucitaj_CVOR()
            READ(jed_Datoteke, *)
        ENDDO 

        !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        !DO i=1, 10    !samo transformator: do 1, 12
        !    READ(jed_Datoteke, *)
        !ENDDO
        !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

        !............................................................................................
        !Ucitavanje podataka za grane

        READ (jed_Datoteke, '(/, 6x, I6)', ERR=101) broj_Grana

        
        DO i=1, 3
            READ(jed_Datoteke, *)
        END DO

                                                          !Alokacija memorije za grane
        IF ( .NOT.ALLOCATED(grane) ) ALLOCATE ( grane(broj_Grana), STAT = Alloc_Error )
        IF (Alloc_Error .NE. 0) THEN
            STOP ("[ERROR] Problem sa alokacijom memorije za grane!")
        END IF


        DO i = 1, broj_Grana
            p_Grana => grane(i)
            Global_Error = p_Grana%Ucitaj_GRANA()
            READ(jed_Datoteke, *)
        ENDDO

        
        !............................................................................................


        CLOSE(jed_Datoteke)


        RETURN

        100 WRITE (*,*)  "[ERROR] Ne postoji ulazna datoteka 'Topol_Podaci' na odgovarajucoj lokaciji"
        CLOSE(jed_Datoteke)
        STOP

        101 WRITE (*,*) "[ERROR] Greska u formatu ulazne datoteke 'Topol_Podaci'"
        CLOSE(jed_Datoteke)
        STOP
    
    END SUBROUTINE Ucitaj_Topologiju

    !********************************************************************************************
    !Proracun snaga P0 i Q0 za cvorove

    SUBROUTINE Proracun_P0_Q0_za_CVOR()
    
        
        WRITE (*, *) '  Proracun snaga P0 i Q0 za cvorove...'

        CALL SLEEP(1)

        DO k = 0, broj_Cvorova
            p_Cvor => cvorovi(k)
            Global_Error = p_Cvor%P0_Q0_za_CVOR()
        ENDDO 


    END SUBROUTINE Proracun_P0_Q0_za_CVOR


    !********************************************************************************************
    !Proracun parametara grana

    SUBROUTINE Proracun_Parametara_GRANA()
    
    
        WRITE (*, *) '  Proracun parametara grana...'

        CALL SLEEP(1)
        
        DO k = 1, broj_Grana
            p_Grana => grane(k)
            Global_Error = p_Grana%Parametri_GRANA()
        ENDDO 


    END SUBROUTINE Proracun_Parametara_GRANA


    !********************************************************************************************
    !Inicijalizacija promenljivih tipa INTEGER

    FUNCTION INT_Inicijalizacija(in_vrednost) 


        INTEGER, INTENT(IN) :: in_vrednost
        INTEGER :: INT_Inicijalizacija


        INT_Inicijalizacija = in_vrednost


    END FUNCTION INT_Inicijalizacija


    !********************************************************************************************
    !Inicijalizacija promenljivih tipa REAL

    FUNCTION REAL_Inicijalizacija(in_vrednost) 


        REAL, INTENT(IN) :: in_vrednost
        REAL :: REAL_Inicijalizacija

            
        REAL_Inicijalizacija = in_vrednost


    END FUNCTION REAL_Inicijalizacija

    !********************************************************************************************


END MODULE Topologija_Helper
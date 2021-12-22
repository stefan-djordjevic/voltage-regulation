!******************************************************************************************************
!Ispis dobijenih rezultata

SUBROUTINE Ispis_Konfiguracije_Mreze(jed_Ispisa)

    USE EES_Elementi
    USE EES_Ucitavanje
    USE Topologija
    USE Topologija_Helper
    !USE Bazne_Velicine !@@@@@@@@@@@@@@

    IMPLICIT NONE


    INTEGER, INTENT(IN) :: jed_Ispisa
    INTEGER :: h


    !TIP IZLAZA
    IF (jed_Ispisa .EQ. ID_File) THEN
        !ISPIS U IZLAZNU DATOTEKU
        WRITE (*, *) '  Ispis konfiguracije mreze u izlaznu datoteku...'
        CALL SLEEP(1)

        OPEN (UNIT = jed_Ispisa, FILE = 'IZLAZ_DATOTEKE\IZLAZ__KOFIGURACIJA_MREZE.txt', ACTION = 'WRITE', STATUS = 'UNKNOWN')

    ELSE
        !ISPIS NA EKRAN
        WRITE (*, *) '  Ispis na ekran...'
        CALL SLEEP(1)

        OPEN (UNIT = jed_Ispisa, FILE = 'CON')      !Ekranu dodeljen logicki broj: jed_Ispisa

    END IF               
    

        !..................................................................................................
        !Ispis unete konfiguracije mreze

        WRITE ( jed_Ispisa, '(/, 1x, 40("=") )' )
        WRITE ( jed_Ispisa, '(8x, a)' ) "UNETA KONFIGURACIJA MREZE"
        WRITE ( jed_Ispisa, '(1x, 40("=") )' )
        WRITE ( jed_Ispisa, '(/, 1x, 40("-") )' )
        WRITE ( jed_Ispisa, '(1x, a, 2(5x, a) )') "Br. grane", "GORNJI Cvor", "DONJI Cvor"
        WRITE ( jed_Ispisa, '(1x, 40("-") )' )

        DO h = 1, broj_Grana
            WRITE (jed_Ispisa, 201) h, grane(h)%Get_Gornji_Cvor(), grane(h)%Get_Donji_Cvor()
        ENDDO    
        
        201 FORMAT (2x, i3, 13x, i3, 12x, i3) 

        !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

        !WRITE (jed_Ispisa, '(//, 1x, 66("=") )' )
        !WRITE (jed_Ispisa, '(17x, a)') "REZULTATI PRORACUNA TOKOVA SNAGA"
        !WRITE (jed_Ispisa, '(1x, 66("=") )' ) 


        !..................................................................................................
        !Ispis kompleksnog napona cvorova i njegovog modula

        !WRITE (jed_Ispisa, '(/, 1x, 66("-") )' )
        !WRITE (jed_Ispisa, '(1x, a, 8x, a, 7x, a)') "Br. cvora", "Kompleksni napon [V]", "Moduo napona [kV]"
        !WRITE (jed_Ispisa, '(1x, 66("-") )' )

        !DO h = 0, broj_Cvorova
        !    IF(h == 0) THEN
        !    WRITE (jed_Ispisa, 202) h, cvorovi(h)%V_Cvora * V_bazno(1), ABS(cvorovi(h)%V_Cvora) * V_bazno(1)/1000
        !    ELSE
        !    WRITE (jed_Ispisa, 202) h, cvorovi(h)%V_Cvora * V_bazno(2), ABS(cvorovi(h)%V_Cvora) * V_bazno(2)/1000
        !    ENDIF
        !ENDDO

        !202 FORMAT ( 3x, i3, 8x,'(' f11.4 ',' f11.4 ' )', 6x, f9.4 )


        !..................................................................................................
        !Ispis kompleksne struje grana i njenog modula

        !WRITE (jed_Ispisa, '(/, 1x, 66("-") )' )
        !WRITE (jed_Ispisa, '(1x, a, 8x, a, 8x, a)') "Br. grane", "Kompleksna struja [A]", "Moduo struje [A]"
        !WRITE (jed_Ispisa, '(1x, 66("-") )' )
    
        !DO h = 1, broj_Grana
        !    WRITE (jed_Ispisa, 203) h, grane(h)%J_Grane * I_bazno(1), ABS(grane(h)%J_Grane) * I_bazno(1)
        !ENDDO

        !203 FORMAT ( 3x, i3, 8x, '(' f9.4 ',' f13.4 ' )', 6x, f9.4 )

        !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


    IF (jed_Ispisa .EQ. ID_File) THEN
        CLOSE(jed_Ispisa)
    END IF


END SUBROUTINE Ispis_Konfiguracije_Mreze


!******************************************************************************************************
!Oslobadjanje dinamicke memorije

SUBROUTINE Dealokacija()

    USE EES_Ucitavanje
    USE Topologija_Helper
    USE Bazne_Velicine
    USE Tokovi_Snaga


    IMPLICIT NONE


    INTEGER :: Dealloc_Error


    WRITE (*, *) '  Oslobadjanje dinamicke memorije...'
    WRITE (*, *) ' '

    CALL SLEEP(1)


    IF( ALLOCATED(vodovi) ) DEALLOCATE( vodovi, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za vodove!")
    END IF


    IF( ALLOCATED(potrosaci) ) DEALLOCATE( potrosaci, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za potrosace!")
    END IF


    IF( ALLOCATED(cvorovi) ) DEALLOCATE( cvorovi, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za cvorove!")
    END IF


    IF( ALLOCATED(grane) ) DEALLOCATE( grane, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za grane!")
    END IF


    IF( ALLOCATED(dV_real) ) DEALLOCATE( dV_real, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za debalans realnog dela napona u cvorovima!")
    END IF


    IF( ALLOCATED(dV_imag) ) DEALLOCATE( dV_imag, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za debalans imaginarnog dela napona u cvorovima!")
    END IF

    
    IF( ALLOCATED(prev_V) ) DEALLOCATE( prev_V, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za napone u cvorovima u prethodnoj iteraciji!")
    END IF

    
    IF( ALLOCATED(V_bazno) ) DEALLOCATE( V_bazno, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za bazne napone!")
    END IF

    
    IF( ALLOCATED(transformatori) ) DEALLOCATE( transformatori, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za transformatore!")
    END IF


END SUBROUTINE Dealokacija
MODULE Tokovi_Snaga

USE EES_Elementi
USE EES_Ucitavanje
USE Topologija
USE Topologija_Helper
USE Bazne_Velicine
USE Interoperabilnost


IMPLICIT NONE


!********************************************************************************************

INTERFACE

    FUNCTION COMPLEX_Inicijalizacija(in_vrednost) 

        COMPLEX, INTENT(IN), OPTIONAL :: in_vrednost
        COMPLEX :: COMPLEX_Inicijalizacija

    END FUNCTION COMPLEX_Inicijalizacija


END INTERFACE

!********************************************************************************************


REAL, PARAMETER :: Eps = 1e-6

INTEGER :: br_Iteracija

REAL, ALLOCATABLE    :: dV_real(:)           !Debalans realnog dela napona u cvorovima
REAL, ALLOCATABLE    :: dV_imag(:)           !Debalans imaginarnog dela napona u cvorovima
COMPLEX, ALLOCATABLE :: prev_V(:)

INTEGER  :: vr_nula = 0                !Vrednost za inicijalizaciju 

!================================================================================================
!================================================================================================

CONTAINS


    !********************************************************************************************
    !Procedura za proracun tokova snaga

    SUBROUTINE Proracun_Tokova_Snaga(sat_potr, poz, percent_Sg)

    INTEGER, INTENT(IN) :: sat_potr
    INTEGER, INTENT(IN) :: poz
    REAL, INTENT(IN)    :: percent_Sg


    CLASS(CVOR), POINTER   :: p_Cvor
    CLASS(GRANA), POINTER  :: p_Grana

    !............................................................................................
    !Promenljive vezane za proracun tokova snaga

    INTEGER  :: i, j
    REAL     :: MAX_dV_real, MAX_dV_imag          !Maksimalni debalans realnog i imaginarnog napona u cvorovima, respektivno

    
    br_Iteracija = Inicijalizacija(vr_nula)                            

    !............................................................................................
    !Alokacija memorije za promenljive

    IF ( .NOT.ALLOCATED(dV_real) ) ALLOCATE ( dV_real(0:broj_Cvorova), STAT = Alloc_Error )
        IF (Alloc_Error .NE. 0) THEN
            STOP ("[ERROR] Problem sa alokacijom memorije za debalans aktivnih snaga u cvorovima!")
        END IF

    IF ( .NOT.ALLOCATED(dV_imag) ) ALLOCATE ( dV_imag(0:broj_Cvorova), STAT = Alloc_Error )
        IF (Alloc_Error .NE. 0) THEN
            STOP ("[ERROR] Problem sa alokacijom memorije za debalans reaktivnih snaga u cvorovima!")
        END IF

    IF ( .NOT.ALLOCATED(prev_V) ) ALLOCATE ( prev_V(0:broj_Cvorova), STAT = Alloc_Error )
        IF (Alloc_Error .NE. 0) THEN
            STOP ("[ERROR] Problem sa alokacijom memorije za napone u cvorovima u prethodnoj iteraciji!")
        END IF


    !............................................................................................
    !Inicijalizacija velicina vezanih za cvorove

    DO i=0, broj_Cvorova

        p_Cvor => cvorovi(i)    
        !Poziv f-je "COMPLEX_Inicijalizacija" bez argumenta => f-ja vraca vrednost 0
        !Poziv f-je "COMPLEX_Inicijalizacija" sa argumentom => f-ja vraca vrednost datog argumenta    
        p_Cvor%Y0_sum = COMPLEX_Inicijalizacija()               ![r.j.]
        p_Cvor%V_Cvora = COMPLEX_Inicijalizacija((1, 0))        ![r.j.], inicijalizacija izjednacavanjem sa naponom korena mreze
        prev_V(i) = p_Cvor%V_Cvora

    ENDDO

    
    !............................................................................................
    !Proracun parametara Tr-a za datu poziciju reg. sklopke
    
    CALL TapChanger_SetPosition(poz)
    

    !............................................................................................
    !Proracun sume otocnih admitansi u cvorovima 

    CALL Proracun_Y0_sum()


    !............................................................................................
    !ITERATIVNI POSTUPAK

    DO WHILE ( br_Iteracija .LE. 500 )
        
        br_Iteracija = br_Iteracija + 1

        
        IF ( br_Iteracija == 11 ) THEN
            WRITE(*,*) "[WARNING] Funkcija tokova snaga ne konvergira nakon desete iteracije."
        END IF

        IF ( br_Iteracija == 501 ) THEN
            STOP ("[ERROR] Funkcija tokova snaga ne konvergira nakon 500. iteracije.")
        END IF


        ![1. KORAK] Proracun injektiranih struja za svaki cvor prema relaciji 3.4
        DO i = 0, broj_Cvorova
            p_Cvor => cvorovi(i)
            
            IF (i == 0) THEN
                CALL p_Cvor%Parametri_CVOR(sat_potr, V_bazno(1), percent_Sg)
            ELSE
                CALL p_Cvor%Parametri_CVOR(sat_potr, V_bazno(2), percent_Sg)
            ENDIF

            CALL Proracun_I_inj_Cvora_C ( p_Cvor%Get_S_cons_Cvor(), p_Cvor%Y0_sum, p_Cvor%V_Cvora, p_Cvor%I_inj_Cvora )
        ENDDO


        ![2. KORAK] Proracun struja za svaku granu prema relaciji 3.7
                    !Zamena unazad - pocevsi od grana u poslednjem lejeru
        DO i = broj_Grana, 1, -1
            grane(i)%J_Grane = cvorovi( grane(i)%Get_Donji_Cvor() )%I_inj_Cvora
            DO j=1, broj_Grana
                IF ( grane(i)%Get_Donji_Cvor() == grane(j)%Get_Gornji_Cvor() ) THEN
                    CALL Proracun_J_Grane_C ( grane(i)%J_Grane, grane(j)%J_Grane )   
                END IF
            ENDDO
        ENDDO


        ![3. KORAK] Proracun napona za svaki cvor prema relaciji 3.8
                    !Zamena unapred - pocevsi od cvora u prvom lejeru
        DO i = 1, broj_Grana
            p_Grana => grane(i)
            CALL Proracun_V_Cvora_C ( cvorovi( p_Grana%Get_Gornji_Cvor() )%V_Cvora,  p_Grana%Get_Z_Grana(),  &
                                      p_Grana%J_Grane,  cvorovi(p_Grana%Get_Donji_Cvor())%V_Cvora )
        ENDDO


        !Proracun injektirane snage za svaki cvor
        DO i = 0, broj_Cvorova
            p_Cvor => cvorovi(i)
            CALL Proracun_S_Cvora_C (p_Cvor%V_Cvora, p_Cvor%I_inj_Cvora, p_Cvor%Y0_sum, p_Cvor%V_Cvora, p_Cvor%S_Cvora)

        ENDDO
        

        dV_real = 0
        dV_imag = 0
        MAX_dV_real = 0
        MAX_dV_imag = 0

        !Proracun debalansa napona u svakom cvoru
        DO i = 1, broj_Cvorova

            p_Cvor => cvorovi(i)
            
            dV_real(i) = ABS(REAL( p_Cvor%V_Cvora - prev_V(i) ))
            dV_imag(i) = ABS(IMAG( p_Cvor%V_Cvora - prev_V(i) ))

        ENDDO

        !Uzimanje vrednosti za prethodne napone cvorova kao vrednosti napona cvorova u trenutnoj iteraciji
        DO i = 0, broj_Cvorova
            p_Cvor => cvorovi(i)
            prev_V(i) = p_Cvor%V_Cvora
        ENDDO

        !Maksimalne vrednosti debalansa
        MAX_dV_real = MAXVAL(dV_real)
        MAX_dV_imag = MAXVAL(dV_imag)

        !Ispitivanje uslova KONVERGENCIJE na kraju tekuce iteracije
        IF ( (MAX_dV_real <= Eps) .AND. (MAX_dV_imag <= Eps) ) THEN
            EXIT
        ELSE 
            CYCLE
        END IF


    ENDDO
    !KRAJ ITERATIVNOG POSTUPKA

    
    CALL Proracun_Snage_Grana()

    CALL Proracun_Gubitaka_Snage()

    CALL Proracun_Pada_Napona()


    CALL Vrati_YZ_Tr_na_Inicijalnu_Vr()
    

    END SUBROUTINE Proracun_Tokova_Snaga



    !********************************************************************************************
    !Proracun sume otocnih admitansi u cvorovima

    SUBROUTINE Proracun_Y0_sum()
    
        CLASS(CVOR), POINTER   :: p_Cvor
        CLASS(GRANA), POINTER  :: p_Grana
        
        INTEGER :: i, j
              

        DO i=0, broj_Cvorova
            DO j=1, broj_Grana
                
                p_Grana => grane(j)
                SELECT CASE(p_Grana%Get_tip_Grane())
                
                    CASE(eVod)

                        IF( (grane(j)%Get_Donji_Cvor() .EQ. i) .or. (grane(j)%Get_Gornji_Cvor() == i) ) THEN
                            
                            cvorovi(i)%Y0_sum = cvorovi(i)%Y0_sum + 0.5 * ( grane(j)%Get_Y_Grana() )

                        ENDIF

                    CASE(eTransformator)

                        IF(grane(j)%Get_Gornji_Cvor() .EQ. i) THEN
                            
                            cvorovi(i)%Y0_sum = cvorovi(i)%Y0_sum + grane(j)%Y0_gornje 
                        
                        ELSE IF(grane(j)%Get_Donji_Cvor() .EQ. i) THEN

                            cvorovi(i)%Y0_sum = cvorovi(i)%Y0_sum + grane(j)%Y0_donje 

                        ENDIF
            
                    CASE DEFAULT
                    STOP ("[ERROR] Pogresan podatak za tip grane!")

                END SELECT       
        
            ENDDO
        ENDDO


    END SUBROUTINE Proracun_Y0_sum


    !********************************************************************************************
    
    SUBROUTINE TapChanger_SetPosition(t)

        INTEGER, INTENT(IN) :: t
        REAL :: a_var, a
        INTEGER :: i

        !Promenljivi deo odnosa transformacije RT-a
        a_var = 1 + t * transformatori(1)%Get_dV_Tap() / 100

        !Odnos transformacije RT-a
        a = a_var * transformatori(1)%Get_V_prim_Nom() / transformatori(1)%Get_V_sek_Nom()

        !Normalizacija odnosa transformacije RT-a
        a = a / (V_bazno(1) / V_bazno(2))
        
        !............................................
        !Proracun otocnih admitansi i redne admitanse i impedanse transformatora

        DO i=1, broj_Grana
        
            IF (grane(i)%Get_tip_Grane() == 2) THEN

                grane(i)%Y0_gornje = (1-a) * grane(i)%Get_Y_Grana()
                grane(i)%Y0_donje = a*(a-1) * grane(i)%Get_Y_Grana()
                !(1-a) * grane(i)%Get_Y_Grana()

                CALL grane(i)%Set_Y_Grana( a * grane(i)%Get_Y_Grana() )
                CALL grane(i)%Set_Z_Grana( 1 / grane(i)%Get_Y_Grana() )

            ENDIF
        
        ENDDO
         
    END SUBROUTINE TapChanger_SetPosition

    !********************************************************************************************

    SUBROUTINE Vrati_YZ_Tr_na_Inicijalnu_Vr()
    
        INTEGER :: i

        DO i=1, broj_Grana
        
            IF (grane(i)%Get_tip_Grane() == 2) THEN

                CALL grane(i)%Set_Y_Grana( Y_Tr_inic )
                CALL grane(i)%Set_Z_Grana( Z_Tr_inic )

            ENDIF
        
        ENDDO

    END SUBROUTINE Vrati_YZ_Tr_na_Inicijalnu_Vr

    !********************************************************************************************
    !Proracun kompleksne snage po granama

    SUBROUTINE Proracun_Snage_Grana()
    
        
        INTEGER :: i
        CLASS(GRANA), POINTER  :: p_Grana


        DO i = 1, broj_Grana
            
            p_Grana => grane(i)
                !SELECT CASE(p_Grana%Get_tip_Grane())
                
                    !CASE(eVod)

                        !p_Grana%S_12_Grane = cvorovi( p_Grana%Get_Gornji_Cvor() )%V_Cvora * CONJG( p_Grana%J_Grane )

                    !CASE(eTransformator)

                        !p_Grana%S_12_Grane = cvorovi( p_Grana%Get_Donji_Cvor() )%V_Cvora * CONJG( p_Grana%J_Grane )
            
                    !CASE DEFAULT
                    !STOP ("[ERROR] Pogresan podatak za tip grane!")

                !END SELECT           
            
            p_Grana => grane(i)
            p_Grana%S_12_Grane = cvorovi( p_Grana%Get_Gornji_Cvor() )%V_Cvora * CONJG( p_Grana%J_Grane )
        ENDDO
        

    END SUBROUTINE Proracun_Snage_Grana
    
    
    !********************************************************************************************
    !Proracun gubitaka aktivne snage u svakoj grani i ukupnih gubitaka aktivne snage mreze

    SUBROUTINE Proracun_Gubitaka_Snage()
    
        
        INTEGER :: i
        CLASS(GRANA), POINTER  :: p_Grana
        REAL :: rezistansa
        REAL :: struja_Moduo


        rezistansa = 0.0
        struja_Moduo = 0.0


        DO i = 1, broj_Grana

            p_Grana => grane(i)
            rezistansa = REAL( p_Grana%Get_Z_Grana() )
            struja_Moduo = ABS( p_Grana%J_Grane )

            p_Grana%Pg_Grane = rezistansa * struja_Moduo**2     

        ENDDO
        

    END SUBROUTINE Proracun_Gubitaka_Snage

    !********************************************************************************************
    !Proracun pada napona u svakoj grani

    SUBROUTINE Proracun_Pada_Napona()
    
        
        INTEGER :: i
        CLASS(GRANA), POINTER  :: p_Grana


        DO i = 1, broj_Grana
            p_Grana => grane(i)
            p_Grana%dV_Grane = cvorovi( p_Grana%Get_Gornji_Cvor() )%V_Cvora - cvorovi( p_Grana%Get_Donji_Cvor() )%V_Cvora
        ENDDO
        

    END SUBROUTINE Proracun_Pada_Napona
    
    
    !********************************************************************************************

END MODULE Tokovi_Snaga


!================================================================================================
!Inicijalizacija promenljivih tipa COMPLEX (sa OPTIONAL argumentom)

FUNCTION COMPLEX_Inicijalizacija(in_vrednost) 


    COMPLEX, INTENT(IN), OPTIONAL :: in_vrednost
    COMPLEX :: COMPLEX_Inicijalizacija


    IF ( PRESENT(in_vrednost) ) THEN
        COMPLEX_Inicijalizacija = in_vrednost
    ELSE
        COMPLEX_Inicijalizacija = 0
    END IF


END FUNCTION COMPLEX_Inicijalizacija

!================================================================================================


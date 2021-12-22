MODULE Bazne_Velicine


USE, INTRINSIC :: ISO_C_BINDING
USE EES_Elementi, ONLY: ID_File
USE EES_Ucitavanje, ONLY: i
USE Topologija
USE Topologija_Helper

IMPLICIT NONE


!********************************************************************************************
!Promenljive vezane za proracun baznih velicina

!Osnovne bazne velicine
REAL :: S_bazno                  ![VA]
REAL, ALLOCATABLE :: V_bazno(:)  ![V]

!Izvedene bazne velicine
REAL, DIMENSION(2) :: I_bazno        ![A]
REAL, DIMENSION(2) :: Z_bazno        ![Om]
REAL, DIMENSION(2) :: Y_bazno        ![S]


!********************************************************************************************

COMPLEX  :: Y_Tr_inic
COMPLEX  :: Z_Tr_inic


!================================================================================================
!================================================================================================

CONTAINS


    !********************************************************************************************
    !Ucitavanje podataka za bazne velicine iz ulazne datoteke

    FUNCTION Ucitaj_Bazne_Velicine() RESULT(Error)


        LOGICAL :: Error

        Error = .false.


        WRITE (*, *) '  Ucitavanje podataka za bazne velicine...'

        CALL SLEEP(1)


        IF ( .NOT.ALLOCATED(V_bazno) ) ALLOCATE ( V_bazno(2), STAT = Alloc_Error )
        IF (Alloc_Error .NE. 0) THEN
            STOP ("[ERROR] Problem sa alokacijom memorije za bazne napone!")
        END IF


        OPEN (UNIT = ID_File, FILE='ULAZ_DATOTEKE\ULAZ_Bazne_Vrednosti.txt', ACTION = 'READ', STATUS = 'UNKNOWN', ERR = 100)

        
        READ (ID_File, '(17x, e12.1)', ERR=101) S_bazno
        
        DO i=1, SIZE(V_bazno)
            READ (ID_File, '(17x, e12.1)', ERR=101) V_bazno(i)
        ENDDO


        CLOSE(ID_File)


        RETURN

        100 WRITE (*,*)  "[ERROR] Ne postoji ulazna datoteka 'Bazne_Vrednosti' na odgovarajucoj lokaciji!"
        Error = .true.
        STOP

        101 WRITE (*,*) "[ERROR] Greska u formatu ulazne datoteke 'Bazne_Vrednosti'!"
        Error = .true.
        STOP


    END FUNCTION Ucitaj_Bazne_Velicine

    !********************************************************************************************
    !Proracun izvedenih baznih velicina

    SUBROUTINE Proracun_Izvedenih_Baznih_Velicina()
    
        
        WRITE (*, *) '  Proracun izvedenih baznih velicina...'          !prema relacijama 3.1 iz dokumentacije

        CALL SLEEP(1)

        DO i=1, 2
            I_bazno(i) = S_bazno / (SQRT(3.) * V_bazno(i)) 
            Z_bazno(i) = (V_bazno(i) * V_bazno(i)) / S_bazno
            Y_bazno(i) = S_bazno / (V_bazno(i)**2) 
        ENDDO  

    END SUBROUTINE Proracun_Izvedenih_Baznih_Velicina

    !********************************************************************************************
    !Pretvaranje velicina vezanih za CVOROVE I GRANE iz apsolutnih u relativne jedinice i obrnuto

    SUBROUTINE Pretvarac_APS_REL(tip_Pretvaraca)


    INTEGER, INTENT(IN) :: tip_Pretvaraca

    CLASS(CVOR), POINTER  :: p_Cvor
    CLASS(GRANA), POINTER :: p_Grana

    INTEGER :: j

    SELECT CASE (tip_Pretvaraca)

        !.........................................................
        !Pretvaranje iz APSOLUTNIH u RELATIVNE jedninice, relacija 3.2 iz dokumentacije
        
        CASE (APS_u_REL)   
        
            WRITE (*, *) '  Pretvaranje velicina iz apsolutnih u relativne jedinice...'                                    
            
            CALL SLEEP(1)

            !velicine cvorova
            DO i = 0, broj_Cvorova
                
                p_Cvor => cvorovi(i)

                !p_Cvor%V_Cvora = p_Cvor%V_Cvora / V_bazno                   
                !p_Cvor%I_inj_Cvora = p_Cvor%I_inj_Cvora / I_bazno           
                !p_Cvor%S_Cvora = p_Cvor%S_Cvora / S_bazno                   

                DO j=1, 24
                    p_Cvor%P0_Cvora(j) = p_Cvor%P0_Cvora(j) * 1000000 / S_bazno
                    p_Cvor%Q0_Cvora(j) = p_Cvor%Q0_Cvora(j) * 1000000 / S_bazno 
                ENDDO                      

            ENDDO

            !velicine grana
            DO i = 1, broj_Grana
                
                p_Grana => grane(i)

                !p_Grana%J_Grane = p_Grana%J_Grane / I_bazno           
                
                SELECT CASE (grane(i)%Get_tip_Grane())

                    CASE (eVod)

                        CALL p_Grana%Set_Z_Grana( p_Grana%Get_Z_Grana() / Z_bazno(2) )  
                        CALL p_Grana%Set_Y_Grana( p_Grana%Get_Y_Grana() / Y_bazno(2) )

                    CASE(eTransformator)

                        CALL p_Grana%Set_Z_Grana( p_Grana%Get_Z_Grana() / Z_bazno(1) )  
                        CALL p_Grana%Set_Y_Grana( p_Grana%Get_Y_Grana() / Y_bazno(1) )
            
                    CASE DEFAULT
                    STOP ("[ERROR] Pogresan podatak za tip grane!")

                END SELECT                      

            ENDDO

            Y_Tr_inic = grane(1)%Get_Y_Grana()
            Z_Tr_inic = grane(1)%Get_Z_Grana()
        
        !.........................................................
        !Pretvaranje iz RELATIVNIH u APSOLUTNE jedinice, relacija 3.3 iz dokumentacije

        CASE (REL_u_APS)  
                                            
            WRITE (*, *) '    Pretvaranje velicina iz relativnih u apsolutne jedinice...' 

            CALL SLEEP(1)
            
            !velicine cvorova
            DO i = 0, broj_Cvorova
                
                p_Cvor => cvorovi(i)

                IF (i == 0) THEN
                    p_Cvor%V_Cvora = p_Cvor%V_Cvora * V_bazno(1)               
                    p_Cvor%I_inj_Cvora = p_Cvor%I_inj_Cvora * I_bazno(1)       
                    !p_Cvor%S_Cvora = p_Cvor%S_Cvora * S_bazno
                ELSE
                    p_Cvor%V_Cvora = p_Cvor%V_Cvora * V_bazno(2)               
                    p_Cvor%I_inj_Cvora = p_Cvor%I_inj_Cvora * I_bazno(2)       
                    !p_Cvor%S_Cvora = p_Cvor%S_Cvora * S_bazno               
                ENDIF
                
                CALL p_Cvor%Set_P_cons_Cvor( p_Cvor%Get_P_cons_Cvor() / 1000000 * S_bazno ) 
                CALL p_Cvor%Set_Q_cons_Cvor( p_Cvor%Get_Q_cons_Cvor() / 1000000 * S_bazno )
                CALL p_Cvor%Set_S_cons_Cvor( p_Cvor%Get_S_cons_Cvor() / 1000000 * S_bazno ) 

            ENDDO

            !velicine grana
            DO i = 1, broj_Grana
                
                p_Grana => grane(i)

                SELECT CASE(grane(i)%Get_tip_Grane())

                    CASE(eVod)

                        p_Grana%J_Grane = p_Grana%J_Grane * I_bazno(2)
                        !p_Grana%S_12_Grane = p_Grana%S_12_Grane * S_bazno
                        !p_Grana%Pg_Grane = p_Grana%Pg_Grane * S_bazno         

                        CALL p_Grana%Set_Z_Grana( p_Grana%Get_Z_Grana() * Z_bazno(2) )  
                        CALL p_Grana%Set_Y_Grana( p_Grana%Get_Y_Grana() * Y_bazno(2) )

                    CASE(eTransformator)

                        p_Grana%J_Grane = p_Grana%J_Grane * I_bazno(1)
                        !p_Grana%S_12_Grane = p_Grana%S_12_Grane * S_bazno
                        !p_Grana%Pg_Grane = p_Grana%Pg_Grane * S_bazno         

                        CALL p_Grana%Set_Z_Grana( p_Grana%Get_Z_Grana() * Z_bazno(1) )  
                        CALL p_Grana%Set_Y_Grana( p_Grana%Get_Y_Grana() * Y_bazno(1) )
            
                    CASE DEFAULT
                    STOP ("[ERROR] Pogresan podatak za tip grane!")

                END SELECT 

            ENDDO

        !.........................................................

        CASE DEFAULT
            STOP ("[ERROR] Pogresan podatak za tip pretvaraca!")

        END SELECT


    END SUBROUTINE Pretvarac_APS_REL

    !********************************************************************************************

END MODULE Bazne_Velicine
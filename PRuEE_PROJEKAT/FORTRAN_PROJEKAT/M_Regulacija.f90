MODULE Regulacija

USE EES_Elementi
USE Topologija
USE Topologija_Helper
USE Bazne_Velicine
USE Tokovi_Snaga

IMPLICIT NONE


REAL :: Imin, Imax, Vmin, Vmax, Kn


CONTAINS


    SUBROUTINE Zakon_Regulacije_ARN()

        
        INTEGER :: h, t, i

        INTEGER :: h_Imax       !sat u koji se ima Imax
        INTEGER :: h_Imin       !sat u koji se ima Imin
        INTEGER :: h_Vmax       !sat u koji se ima Vmax
        INTEGER :: h_Vmin       !sat u koji se ima Vmin

        REAL :: STETA           !ukupna steta za sve cvorove pri odredjenom satu potrosnje i poziciji reg. sklopke
        REAL :: min_STETA       !minimalna uk. steta od svih pozicija reg. sklopke za odredjeni sat potrosnje
        REAL :: steta_24h
        
        INTEGER :: t_opt_sat    !pozicija reg. sklopke pri kojoj se ima minimalna steta za odredjeni sat potrosnje

        REAL :: I_tr_sat, V_opt_sat
        
        LOGICAL :: USLOV_I, USLOV_U, USLOV_S 
        
        WRITE (*, *) '  Odredjivanje Zakona Regulacije ARN-a...'
        CALL SLEEP(1)
        
        USLOV_I = .true. 
        USLOV_U = .true.
        USLOV_S = .true.  


        WRITE(33, '( 41("-") )')
        WRITE(33, '( 1x, a, 4(2x, a) )') "T[h]", "Itr[A]", "Vopt[kV]", "t_opt", "minSteta"
        WRITE(33, '( 41("-") )')

        steta_24h = 0
        
        DO h=1, 24    
        
            DO t = transformatori(1)%Get_min_Tap_Pos(), transformatori(1)%Get_max_Tap_Pos() 

                STETA = 0

                CALL Proracun_Tokova_Snaga(h, t, 0.0)

                CALL Ispis_Tokovi_Snaga("zakon regulacije:    ", h, t)

                DO i = 3, broj_Cvorova
                    STETA = STETA + (( (cvorovi(i)%P0_Cvora(h) * S_bazno) / 1000 * (( (ABS(cvorovi(i)%V_Cvora) * V_bazno(2)) - 20000 ) /1000)**2 ) * 5)
                    !Get_P_cons_Cvor()
                    !P0_Cvora(h)
                ENDDO
                    
                IF (USLOV_S) THEN
                        
                    USLOV_S = .false.
                    min_STETA = STETA
                    t_opt_sat = t
                    I_tr_sat = ABS(ABS(grane(3)%J_Grane) - ABS(grane(2)%J_Grane))
                    V_opt_sat = ABS(cvorovi(1)%V_Cvora)
                            
                ELSE IF ( min_STETA > STETA ) THEN

                    min_STETA = STETA
                    t_opt_sat = t
                    I_tr_sat = ABS(ABS(grane(3)%J_Grane) - ABS(grane(2)%J_Grane))
                    V_opt_sat = ABS(cvorovi(1)%V_Cvora)

                ENDIF  
            
            ENDDO

            WRITE(33, 207) h, I_tr_sat * I_bazno(2), V_opt_sat * V_bazno(2) / 1000, t_opt_sat, min_STETA
            
            207 FORMAT ( 2x, i2, 2x, f7.2, 3x, f6.3, 4x, i3, 3x, f6.1 ) 
            
            USLOV_S = .true.

            steta_24h = steta_24h + min_STETA

            IF (USLOV_I) THEN
                
                USLOV_I = .false.
                Imin = I_tr_sat
                Imax = I_tr_sat
                h_Imax = h
                h_Imin = h
                Vmin = V_opt_sat
                Vmax = V_opt_sat
                h_Vmax = h
                h_Vmin = h

            ELSE IF ( Imax < I_tr_sat ) THEN
                
                Imax = I_tr_sat
                h_Imax = h
                Vmax = V_opt_sat
                h_Vmax = h

            ELSE IF ( I_tr_sat < Imin ) THEN

                Imin = I_tr_sat
                h_Imin = h
                Vmin = V_opt_sat
                h_Vmin = h

            ENDIF

            
            !IF (USLOV_U) THEN
                
                !USLOV_U = .false.
                !Vmin = V_opt_sat
                !Vmax = V_opt_sat
                !h_Vmax = h
                !h_Vmin = h

            !ELSE IF ( Vmax < V_opt_sat ) THEN
                
                !Vmax = V_opt_sat
                !h_Vmax = h

            !ELSE IF ( V_opt_sat < Vmin ) THEN

                !Vmin = V_opt_sat
                !h_Vmin = h

            !ENDIF

        ENDDO

        WRITE(33, '( 41("-") )')
        WRITE(33, 208) "Imin=", Imin * I_bazno(2), "h=", h_Imin, "Vmin=", Vmin * V_bazno(2) / 1000, "h=", h_Vmin
        WRITE(33, 208) "Imax=", Imax * I_bazno(2), "h=", h_Imax, "Vmax=", Vmax * V_bazno(2) / 1000, "h=", h_Vmax
        WRITE(33, '( 41("-") )')
        WRITE(33, '( a, f7.1 )') "Ukupna steta =", steta_24h
        WRITE(33, '( 41("-") )')

        208 FORMAT ( 1x, a, f7.2, 1x, a, i2, 2x, a, f6.3, 1x, a, i2 )

        USLOV_I = .true. 
        USLOV_U = .true.


        Kn = (Vmax - Vmin) / (Imax - Imin)
    
    
    END SUBROUTINE Zakon_Regulacije_ARN

    !---------------------------------------------------------------------------------------------

    SUBROUTINE REGULACIJA_NAPONA()
      
        LOGICAL :: USLOV_R

        INTEGER :: p, h, s
        REAL    :: sg

        REAL :: min_delta_V_reg
        REAL :: Vopt, I_tr_avrg
        INTEGER :: t_opt
        REAL :: V_ostv_sn, V_p5, I_tr
        REAL :: STETA_g, STETA_g_opt
        REAL :: steta_24h_g
        
        WRITE (*, *) ' REGULACIJA NAPONA JE POKRENUTA...'
        CALL SLEEP(1)

        
        OPEN (UNIT = 22, FILE = 'IZLAZ_DATOTEKE\Kontrolni_Fajl_Tokovi_Snaga.txt', ACTION = 'WRITE', STATUS = 'UNKNOWN')

        OPEN (UNIT = 33, FILE = 'IZLAZ_DATOTEKE\Kontrolni_Fajl_Zakon_Regulacije.txt', ACTION = 'WRITE', STATUS = 'UNKNOWN')

        OPEN (UNIT = 51, FILE = 'IZLAZ_DATOTEKE\IZLAZ__REG_Isn.txt', ACTION = 'WRITE', STATUS = 'UNKNOWN') 

        OPEN (UNIT = 52, FILE = 'IZLAZ_DATOTEKE\IZLAZ__REG_Uost.sn.txt', ACTION = 'WRITE', STATUS = 'UNKNOWN') 

        
        CALL Zakon_Regulacije_ARN()

        
        WRITE (*, *) '  Postavljanje reg. slopke Tr-a na optimalne pozicije i ispis...'
        CALL SLEEP(1)
        
        
        OPEN (UNIT = ID_File, FILE = 'IZLAZ_DATOTEKE\IZLAZ__REGULACIJA.txt', ACTION = 'WRITE', STATUS = 'UNKNOWN') 
        
        OPEN (UNIT = 44, FILE = 'IZLAZ_DATOTEKE\IZLAZ__REGULACIJA(sa stetom).txt', ACTION = 'WRITE', STATUS = 'UNKNOWN') 

        
        DO s = 0, 10
            
            sg = 0.1 * s
            
            CALL Ispis_Snaga_Generatora(sg)

            steta_24h_g = 0

            DO h = 1, 24             

                CALL Proracun_Tokova_Snaga(h, 0, sg)

                CALL Ispis_Tokovi_Snaga("REGULACIJA - Vopt    ", h, 0)

                I_tr_avrg = ABS(ABS(grane(3)%J_Grane) - ABS(grane(2)%J_Grane))

                Vopt = Kn * ( I_tr_avrg - Imin ) + Vmin
                
                USLOV_R = .true.
                
                DO p = transformatori(1)%Get_min_Tap_Pos(), transformatori(1)%Get_max_Tap_Pos() 
                    
                    CALL Proracun_Tokova_Snaga(h, p, sg)

                    CALL Ispis_Tokovi_Snaga("REGULACIJA - V_ost_sn", h, p)

                    STETA_g = 0
                    
                    IF (USLOV_R) THEN
                        
                        USLOV_R = .false.
                        min_delta_V_reg = ABS( Vopt - ABS(cvorovi(1)%V_Cvora) )
                        t_opt = p
                        V_ostv_sn = ABS(cvorovi(1)%V_Cvora)
                        V_p5 = ABS(cvorovi(broj_Cvorova)%V_Cvora)
                        I_tr = ABS(ABS(grane(3)%J_Grane) - ABS(grane(2)%J_Grane))
                        
                        DO i = 3, broj_Cvorova
                            STETA_g = STETA_g + (( (cvorovi(i)%P0_Cvora(h) * S_bazno) / 1000 * (( (ABS(cvorovi(i)%V_Cvora) * V_bazno(2)) - 20000 ) /1000)**2 ) * 5)
                        ENDDO

                        STETA_g_opt = STETA_g
                            
                     ELSE IF ( min_delta_V_reg > ABS( Vopt - ABS(cvorovi(1)%V_Cvora) ) ) THEN

                        min_delta_V_reg = ABS( Vopt - ABS(cvorovi(1)%V_Cvora) )
                        t_opt = p
                        V_ostv_sn = ABS(cvorovi(1)%V_Cvora)
                        V_p5 = ABS(cvorovi(broj_Cvorova)%V_Cvora)
                        I_tr = ABS(ABS(grane(3)%J_Grane) - ABS(grane(2)%J_Grane))

                        DO i = 3, broj_Cvorova
                            STETA_g = STETA_g + (( (cvorovi(i)%P0_Cvora(h) * S_bazno) / 1000 * (( (ABS(cvorovi(i)%V_Cvora) * V_bazno(2)) - 20000 ) /1000)**2 ) * 5)
                        ENDDO

                        STETA_g_opt = STETA_g

                    ENDIF

                ENDDO

                steta_24h_g = steta_24h_g + STETA_g_opt

                CALL Ispis_Regulacija( h, I_tr, Vopt, t_opt, V_ostv_sn, V_p5 )

                CALL Ispis_Regulacija( h, I_tr, Vopt, t_opt, V_ostv_sn, V_p5, STETA_g_opt )      !sa stetom

                CALL Ispis_Regulacija_COPY_PASTE(I_tr, V_ostv_sn)

            ENDDO

            WRITE(44, '( 59("-") )')
            WRITE(44, '( a, f10.1 )') "Ukupna steta =", steta_24h_g
        
        ENDDO

        
        CLOSE(ID_File)

        CLOSE(44)

        CLOSE(33)

        CLOSE(22)

        CLOSE(51)

        CLOSE(52)


    END SUBROUTINE REGULACIJA_NAPONA

    !---------------------------------------------------------------------------------------------

    SUBROUTINE Ispis_Regulacija(hh, Itr, Vop, t_o, Vsn, V5, stetaa)

        INTEGER, INTENT(IN) :: hh
        REAL, INTENT(IN) :: Itr
        REAL, INTENT(IN) :: Vop
        INTEGER, INTENT(IN) :: t_o
        REAL, INTENT(IN) :: Vsn
        REAL, INTENT(IN) :: V5
        REAL, INTENT(IN), OPTIONAL :: stetaa


        IF ( PRESENT(stetaa) ) THEN
            WRITE(44, 515) hh, Itr * I_bazno(2), Vop * V_bazno(2) / 1000, t_o, Vsn * V_bazno(2) / 1000, V5 * V_bazno(2) / 1000, stetaa
        ELSE
            WRITE(ID_File, 501) hh, Itr * I_bazno(2), Vop * V_bazno(2) / 1000, t_o, Vsn * V_bazno(2) / 1000, V5 * V_bazno(2) / 1000
        ENDIF


        501 FORMAT ( 1x, i2, 3x, f6.1, 3x, f6.3, 5x, i3, 4x, f6.3, 5x, f6.3)
        515 FORMAT ( 1x, i2, 3x, f6.1, 3x, f6.3, 5x, i3, 4x, f6.3, 5x, f6.3, 2x, F7.1)


    END SUBROUTINE Ispis_Regulacija

    !---------------------------------------------------------------------------------------------

    SUBROUTINE Ispis_Snaga_Generatora(ss)
        
        REAL, INTENT(IN) :: ss

        WRITE(ID_File, '( /, 51("=") )' )
        WRITE(ID_File, '( a, f5.1, a )') 'SNAGA GENERATORA: ', ss * ABS(CMPLX(potrosaci(2)%Get_P_0(1), potrosaci(2)%Get_Q_0(1))), '[MW]'
        WRITE(ID_File, '( 51("="), / )' )
        WRITE(ID_File, '( 5(a, 2x), a )') "T[h]", "Isn[A]", "Uz.o.[kV]", "t_opt", "Uo.sn[kV]", "U_p5[kV]"
        WRITE(ID_File, '( 51("-") )' )

        WRITE(44, '( /, 59("=") )' )
        WRITE(44, '( a, f5.1, a )') 'SNAGA GENERATORA: ', ss * ABS(CMPLX(potrosaci(2)%Get_P_0(1), potrosaci(2)%Get_Q_0(1))), '[MW]'
        WRITE(44, '( 59("="), / )' )
        WRITE(44, '( 5(a, 2x), a, 2x, a )') "T[h]", "Isn[A]", "Uz.o.[kV]", "t_opt", "Uo.sn[kV]", "U_p5[kV]", "Steta"
        WRITE(44, '( 59("-") )' )

        WRITE(51, '( /, 59("=") )' )
        WRITE(51, '( a, f5.1, a )') 'SNAGA GENERATORA: ', ss * ABS(CMPLX(potrosaci(2)%Get_P_0(1), potrosaci(2)%Get_Q_0(1))), '[MW]'
        WRITE(51, '( 59("="), / )' )
        WRITE(51, '( a )') "Isn[A]"
        WRITE(51, '( 59("-") )' )

        WRITE(52, '( /, 59("=") )' )
        WRITE(52, '( a, f5.1, a )') 'SNAGA GENERATORA: ', ss * ABS(CMPLX(potrosaci(2)%Get_P_0(1), potrosaci(2)%Get_Q_0(1))), '[MW]'
        WRITE(52, '( 59("="), / )' )
        WRITE(52, '( a )') "Uo.sn[kV]"
        WRITE(52, '( 59("-") )' )
        

    END SUBROUTINE Ispis_Snaga_Generatora

    !---------------------------------------------------------------------------------------------

    SUBROUTINE Ispis_Tokovi_Snaga(text, ssat, ppoz)

        INTEGER :: i

        CHARACTER(21), INTENT(IN) :: text
        INTEGER, INTENT(IN)   :: ssat, ppoz
        
        WRITE (22, *)
        WRITE (22, '( 36("=") )' ) 
        WRITE (22, '( 36("=") )' ) 
        WRITE (22, '( a, 1x, a, 1x, a, i2, 1x, a, i3)') "->", text, "h=", ssat, "t=", ppoz  
        WRITE (22, '( 36("-") )' )
        WRITE (22, '(1x, a, 3(2x, a) )') "Cvor", "I_inj[A]", "S_inj[MVA]", "U[kV]"
        WRITE (22, '( 36("-") )' )

        DO i = 0, broj_Cvorova
            IF(i == 0) THEN
                WRITE(22, 202) i, ABS(cvorovi(i)%I_inj_Cvora) * I_bazno(1), ABS(cvorovi(i)%S_Cvora) * S_bazno / 1000000, ABS(cvorovi(i)%V_Cvora) * V_bazno(1)/1000
            ELSE
                WRITE(22, 202) i, ABS(cvorovi(i)%I_inj_Cvora) * I_bazno(2), ABS(cvorovi(i)%S_Cvora) * S_bazno / 1000000, ABS(cvorovi(i)%V_Cvora) * V_bazno(2)/1000
            ENDIF
        ENDDO

        202 FORMAT ( 1x, i2, 4x, f7.2, 6x, f4.2, 4x, f7.3)


        WRITE (22, '(/, 36("-") )' )
        WRITE (22, '( 5(a, 2x) )') "Grana", "I[A]", "S[MVA]", "dP[kW]", "dU[V]"
        WRITE (22, '( 36("-") )' )
    
        DO i = 1, broj_Grana
            IF(i == 1) THEN
                WRITE(22, 203) i, ABS(grane(i)%J_Grane) * I_bazno(1), ABS(grane(i)%S_12_Grane) * S_bazno / 1000000, grane(i)%Pg_Grane * S_bazno / 1000, " -/-/-/"     !ABS(grane(i)%dV_Grane) * V_bazno(2)
            ELSE
                WRITE(22, 204) i, ABS(grane(i)%J_Grane) * I_bazno(2), ABS(grane(i)%S_12_Grane) * S_bazno / 1000000, grane(i)%Pg_Grane * S_bazno / 1000, ABS(grane(i)%dV_Grane) * V_bazno(2)
            ENDIF
        ENDDO

        WRITE (22, '( 36("-") )' )
        WRITE (22, '( a, f7.2 )' ) 'Itr = I(1) - I(2) = ', (ABS(grane(3)%J_Grane) - ABS(grane(2)%J_Grane)) * I_bazno(2)
        WRITE (22, '( 36("-") )' )

        203 FORMAT ( 1x, i2, 2x, f7.2, 2x, f5.2, 2x, f6.2, 1x, a)
        204 FORMAT ( 1x, i2, 2x, f7.2, 2x, f5.2, 2x, f6.2, 1x, f7.2)


    END SUBROUTINE Ispis_Tokovi_Snaga

    !---------------------------------------------------------------------------------------------

    SUBROUTINE Ispis_Regulacija_COPY_PASTE(Itr, Vsn)

        REAL, INTENT(IN) :: Itr
        REAL, INTENT(IN) :: Vsn


        WRITE(51, 519) Itr * I_bazno(2)
        WRITE(52, 520) Vsn * V_bazno(2) / 1000

        519 FORMAT (f6.1)
        520 FORMAT (f6.3)


    END SUBROUTINE Ispis_Regulacija_COPY_PASTE

    !---------------------------------------------------------------------------------------------


END MODULE Regulacija
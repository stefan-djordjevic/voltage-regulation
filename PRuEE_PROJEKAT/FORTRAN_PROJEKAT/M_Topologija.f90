MODULE Topologija


USE EES_Elementi
USE EES_Ucitavanje


IMPLICIT NONE


ENUM, BIND(C)
    ENUMERATOR :: eVod = 1, eTransformator = 2, APS_u_REL = 3, REL_u_APS = 4, PQ = 10, ICOSFI = 20
ENDENUM


!********************************************************************************************
!Korisnicki definisani tip koji modeluje CVOROVE u mrezi

TYPE, PUBLIC :: CVOR
    
    PRIVATE
        INTEGER  :: cvor_ID              !sifra cvora
        REAL     :: P_cons_Cvora         !aktivna snaga potrosnje u cvoru [kW]
        REAL     :: Q_cons_Cvora         !reaktivna snaga potrosnje u cvoru [kVAr]
        COMPLEX  :: S_cons_Cvora         !snaga potrosnje u cvoru [kVA]

    REAL, DIMENSION(24), PUBLIC  :: P0_Cvora
    REAL, DIMENSION(24), PUBLIC  :: Q0_Cvora
    COMPLEX, PUBLIC  :: V_Cvora          !napon cvora [V]
    COMPLEX, PUBLIC  :: Y0_sum           !Suma otocnih admitansi u cvoru [r.j.] 
    COMPLEX, PUBLIC  :: I_inj_Cvora      !injektirana struja u cvoru [A]
    COMPLEX, PUBLIC  :: S_Cvora          !injektirana snaga cvora [VA]


    CONTAINS

        PROCEDURE, PUBLIC :: Ucitaj_CVOR        => Ucitaj_podatke_za_CVOR
        PROCEDURE, PUBLIC :: P0_Q0_za_CVOR
        PROCEDURE, PUBLIC :: Parametri_CVOR     => Proracun_parametara_za_CVOR
        
        PROCEDURE, PUBLIC :: Get_CVOR_ID        => Uzmi_ID_za_CVOR
        PROCEDURE, PUBLIC :: Get_P_cons_Cvor    => Uzmi_P_cons_za_CVOR
        PROCEDURE, PUBLIC :: Get_Q_cons_Cvor    => Uzmi_Q_cons_za_CVOR
        PROCEDURE, PUBLIC :: Get_S_cons_Cvor    => Uzmi_S_cons_za_CVOR
        
        PROCEDURE, PUBLIC :: Set_P_cons_Cvor    => Dodeli_vrednost_za_P_cons_za_CVOR
        PROCEDURE, PUBLIC :: Set_Q_cons_Cvor    => Dodeli_vrednost_za_Q_cons_za_CVOR
        PROCEDURE, PUBLIC :: Set_S_cons_Cvor    => Dodeli_vrednost_za_S_cons_za_CVOR


END TYPE CVOR

PRIVATE :: Ucitaj_podatke_za_CVOR, Proracun_parametara_za_CVOR, Uzmi_ID_za_CVOR
PRIVATE :: Uzmi_P_cons_za_CVOR, Uzmi_Q_cons_za_CVOR, Uzmi_S_cons_za_CVOR
PRIVATE :: Dodeli_vrednost_za_P_cons_za_CVOR, Dodeli_vrednost_za_Q_cons_za_CVOR, Dodeli_vrednost_za_S_cons_za_CVOR

!********************************************************************************************
!Korisnicki definisani tip koji modeluje GRANE u mrezi

TYPE, PUBLIC:: GRANA
    
    PRIVATE
        INTEGER  :: grana_ID             !sifra grane
        INTEGER  :: gornji_Cvor          !cvor grane blizi izvoru napajanja
        INTEGER  :: donji_Cvor           !cvor grane udaljeniji od izvora napajanja
        COMPLEX  :: Z_Grane              !impedansa grane [Om]
        COMPLEX  :: Y_Grane              !admitansa grane [S]
        INTEGER  :: tip_Grane 

    COMPLEX, PUBLIC  :: J_Grane          !struja grane [A]
    COMPLEX, PUBLIC  :: S_12_Grane       !kompleksna snaga grane [VA]
    REAL, PUBLIC     :: Pg_Grane         !gubici aktivne snage grane [W]
    COMPLEX, PUBLIC  :: dV_Grane         !pad napona u grani [V]
    COMPLEX, PUBLIC  :: Y0_gornje = 0
    COMPLEX, PUBLIC  :: Y0_donje = 0


    CONTAINS

        PROCEDURE, PUBLIC :: Ucitaj_GRANA       => Ucitaj_podatke_za_GRANU
        PROCEDURE, PUBLIC :: Parametri_GRANA    => Proracun_parametara_za_GRANU

        PROCEDURE, PUBLIC :: Get_GRANA_ID       => Uzmi_ID_za_GRANU
        PROCEDURE, PUBLIC :: Get_Gornji_Cvor    => Uzmi_podatak_za_GORNJI_CVOR_Grane
        PROCEDURE, PUBLIC :: Get_Donji_Cvor     => Uzmi_podatak_za_DONJI_CVOR_Grane
        PROCEDURE, PUBLIC :: Get_Z_Grana        => Uzmi_Z_za_GRANU
        PROCEDURE, PUBLIC :: Get_Y_Grana        => Uzmi_Y_za_GRANU
        PROCEDURE, PUBLIC :: Get_tip_Grane        

        PROCEDURE, PUBLIC :: Set_Z_Grana        => Dodeli_vrednost_za_Z_za_GRANU
        PROCEDURE, PUBLIC :: Set_Y_Grana        => Dodeli_vrednost_za_Y_za_GRANU
   

END TYPE GRANA

PRIVATE :: Ucitaj_podatke_za_GRANU, Proracun_parametara_za_GRANU, Uzmi_ID_za_GRANU
PRIVATE :: Uzmi_podatak_za_GORNJI_CVOR_Grane, Uzmi_podatak_za_DONJI_CVOR_Grane
PRIVATE :: Uzmi_Z_za_GRANU, Uzmi_Y_za_GRANU
PRIVATE :: Dodeli_vrednost_za_Z_za_GRANU, Dodeli_vrednost_za_Y_za_GRANU


!================================================================================================
!================================================================================================

CONTAINS

    !********************************************************************************************

    FUNCTION Ucitaj_podatke_za_CVOR(this) RESULT(Error)

        CLASS (CVOR) :: this
        LOGICAL      :: Error

        Error = .false.

        READ (ID_File, '(6x, I9)', ERR=101) this%cvor_ID

        RETURN

        101 WRITE (*,*) "[ERROR] Greska u formatu ulazne datoteke 'Topol_Podaci!'"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_podatke_za_CVOR

    !********************************************************************************************

    FUNCTION Ucitaj_podatke_za_GRANU(this) RESULT(Error)

        CLASS (GRANA) :: this
        LOGICAL       :: Error

        Error = .false.

        READ (ID_File, '(6x, 4(I12, 2x))', ERR=101) this%grana_ID, this%gornji_Cvor, this%donji_Cvor, this%tip_Grane

        RETURN

        101 WRITE (*,*) "[ERROR] Greska u formatu ulazne datoteke 'Topol_Podaci'!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_podatke_za_GRANU

    !********************************************************************************************

    FUNCTION Uzmi_ID_za_CVOR (this) RESULT(ID_Cvora)


        CLASS(CVOR)  :: this
        INTEGER      :: ID_Cvora

        ID_Cvora = this%cvor_ID
    

    END FUNCTION Uzmi_ID_za_CVOR

    !********************************************************************************************

    FUNCTION Uzmi_ID_za_GRANU (this) RESULT(ID_Grane)


        CLASS(GRANA) :: this
        INTEGER      :: ID_Grane

        ID_Grane = this%grana_ID
    

    END FUNCTION Uzmi_ID_za_GRANU

    !********************************************************************************************

    FUNCTION Uzmi_podatak_za_GORNJI_CVOR_Grane (this) RESULT(Cvor1_Grane)


        CLASS(GRANA) :: this
        INTEGER      :: Cvor1_Grane

        Cvor1_Grane = this%gornji_Cvor
    

    END FUNCTION Uzmi_podatak_za_GORNJI_CVOR_Grane

    !********************************************************************************************

    FUNCTION Uzmi_podatak_za_DONJI_CVOR_Grane (this) RESULT(Cvor2_Grane)


        CLASS(GRANA) :: this
        INTEGER      :: Cvor2_Grane

        Cvor2_Grane = this%donji_Cvor
    

    END FUNCTION Uzmi_podatak_za_DONJI_CVOR_Grane

    !********************************************************************************************

    FUNCTION Proracun_parametara_za_GRANU(this) RESULT(Error)


        CLASS(GRANA)  :: this
        LOGICAL       :: Error

        INTEGER       :: indeks

        CLASS(VOD), POINTER :: p_Vod
        CLASS(TRANSFORMATOR), POINTER :: p_Transformator

        Error = .false.

        
        SELECT CASE (this%tip_Grane)

            CASE(eVod)

                indeks = Indeks_Na_Osnovu_ID_GRANE_V(this%Grana_ID)

                p_Vod => vodovi(indeks)

                this%Z_Grane = CMPLX ( p_Vod%Get_r_poduzno() * p_Vod%Get_duzina(), p_Vod%Get_x_poduzno() * p_Vod%Get_duzina() ) 
                this%Y_Grane = CMPLX ( p_Vod%Get_g_poduzno() * p_Vod%Get_duzina(), p_Vod%Get_b_poduzno() * p_Vod%Get_duzina() )

            CASE(eTransformator)

                indeks = Indeks_Na_Osnovu_ID_GRANE_T(this%Grana_ID)

                p_Transformator => transformatori(indeks)

                this%Z_Grane = DCMPLX (0, (p_Transformator%Get_u_k()*p_Transformator%Get_V_prim_Nom()**2)/(100*p_Transformator%Get_S_nom()) ) 
                this%Y_Grane = 1 / this%Z_Grane
            
            CASE DEFAULT
                STOP ("[ERROR] Pogresan podatak za tip grane!")

        END SELECT


    END FUNCTION Proracun_parametara_za_GRANU

    !********************************************************************************************

    FUNCTION P0_Q0_za_CVOR(this) RESULT(Error)


        CLASS(CVOR)         :: this
        LOGICAL             :: Error
        INTEGER :: i
        
        INTEGER :: indeks

        CLASS(POTROSAC), POINTER :: p_Potrosac

        Error = .false.

        
        indeks = Indeks_Na_Osnovu_ID_CVORA(this%Cvor_ID)
        p_Potrosac => potrosaci(indeks)


        SELECT CASE (p_Potrosac%Get_tip_Potr())

            CASE(PQ)

                DO i=1,24
                    this%P0_Cvora(i) = p_Potrosac%Get_P_0(i)
                    this%Q0_Cvora(i) = p_Potrosac%Get_Q_0(i)
                ENDDO

            CASE(ICOSFI)

                DO i=1,24
                    this%P0_Cvora(i) = SQRT(3.) * p_Potrosac%Get_I_0(i) * (p_Potrosac%Get_V_nom_Potr() / 1000000) * p_Potrosac%Get_cos_fi_0(i)
                    this%Q0_Cvora(i) = SQRT(3.) * p_Potrosac%Get_I_0(i) * (p_Potrosac%Get_V_nom_Potr() / 1000000) * SQRT(1 - p_Potrosac%Get_cos_fi_0(i)**2) 
                ENDDO

            CASE DEFAULT
                STOP ("[ERROR] Pogresan podatak za tip potrosaca!")

        END SELECT


    END FUNCTION P0_Q0_za_CVOR

    !********************************************************************************************

    SUBROUTINE Proracun_parametara_za_CVOR(this, sat, Vb, p_Sg)


        CLASS(CVOR)         :: this
        INTEGER, INTENT(IN) :: sat
        REAL, INTENT(IN)    :: Vb
        REAL, INTENT(IN)    :: p_Sg    !procentualni deo snage koji se uzima za generator (deljen sa 100)
        
        INTEGER :: indeks
        REAL    :: V_rel

        CLASS(POTROSAC), POINTER :: p_Potrosac

        
        indeks = Indeks_Na_Osnovu_ID_CVORA(this%Cvor_ID)
        p_Potrosac => potrosaci(indeks)

        
        V_rel = ( ABS(this%V_Cvora) * Vb ) / p_Potrosac%Get_V_nom_Potr()

        IF (this%Cvor_ID == 5002) THEN
            this%P_cons_Cvora = this%P0_Cvora(sat) * p_Sg * ( p_Potrosac%Get_Ap() * V_rel**2 + p_Potrosac%Get_Bp() * V_rel + p_Potrosac%Get_Cp() )
            this%Q_cons_Cvora = this%Q0_Cvora(sat) * p_Sg * ( p_Potrosac%Get_Aq() * V_rel**2 + p_Potrosac%Get_Bq() * V_rel + p_Potrosac%Get_Cq() )
        ELSE
            this%P_cons_Cvora = this%P0_Cvora(sat) * ( p_Potrosac%Get_Ap() * V_rel**2 + p_Potrosac%Get_Bp() * V_rel + p_Potrosac%Get_Cp() )
            this%Q_cons_Cvora = this%Q0_Cvora(sat) * ( p_Potrosac%Get_Aq() * V_rel**2 + p_Potrosac%Get_Bq() * V_rel + p_Potrosac%Get_Cq() )
        ENDIF

        this%S_cons_Cvora = CMPLX ( this%P_cons_Cvora, this%Q_cons_Cvora )


    END SUBROUTINE Proracun_parametara_za_CVOR


    !********************************************************************************************

    !============================================================================================

    !********************************************************************************************

    FUNCTION Indeks_Na_Osnovu_ID_GRANE_V (GranaID) RESULT(Indeks)


        INTEGER :: GranaID
        INTEGER :: Indeks

        CLASS(VOD), POINTER :: p_Vod
        INTEGER :: j

        Indeks = 0

        DO j = 1, SIZE(vodovi)
            p_Vod => vodovi(j)
            IF (GranaID == p_Vod%Get_ID()) THEN
                Indeks = j
                EXIT
            END IF
        END DO

    END FUNCTION Indeks_Na_Osnovu_ID_GRANE_V


    FUNCTION Indeks_Na_Osnovu_ID_GRANE_T (GranaID) RESULT(Indeks)


        INTEGER :: GranaID
        INTEGER :: Indeks

        CLASS(TRANSFORMATOR), POINTER :: p_Transformator
        INTEGER :: j

        Indeks = 0

        DO j = 1, SIZE(transformatori)
            p_Transformator => transformatori(j)
            IF (GranaID == p_Transformator%Get_ID()) THEN
                Indeks = j
                EXIT
            END IF
        END DO

    END FUNCTION Indeks_Na_Osnovu_ID_GRANE_T

    !********************************************************************************************

    FUNCTION Indeks_Na_Osnovu_ID_CVORA (CvorID) RESULT(Indeks)


        INTEGER :: CvorID
        INTEGER :: Indeks

        CLASS(POTROSAC), POINTER :: p_Potrosac
        INTEGER :: j

        Indeks = 0

        DO j = 0, SIZE(potrosaci)
            p_Potrosac => potrosaci(j)
            IF (CvorID == p_Potrosac%Get_ID()) THEN
                Indeks = j
                EXIT
            END IF
        END DO

    END FUNCTION Indeks_Na_Osnovu_ID_CVORA

    !********************************************************************************************

    !============================================================================================

    !********************************************************************************************

    FUNCTION Uzmi_P_cons_za_CVOR(this) RESULT(cvor_P_cons)


        CLASS(CVOR) :: this
        REAL        :: cvor_P_cons

        cvor_P_cons = this%P_cons_Cvora


    END FUNCTION Uzmi_P_cons_za_CVOR

    !********************************************************************************************

    FUNCTION Uzmi_Q_cons_za_CVOR(this) RESULT(cvor_Q_cons)


        CLASS(CVOR) :: this
        REAL        :: cvor_Q_cons

        cvor_Q_cons = this%Q_cons_Cvora


    END FUNCTION Uzmi_Q_cons_za_CVOR

    !********************************************************************************************

    FUNCTION Uzmi_S_cons_za_CVOR(this) RESULT(cvor_S_cons)


        CLASS(CVOR) :: this
        COMPLEX     :: cvor_S_cons

        cvor_S_cons = this%S_cons_Cvora


    END FUNCTION Uzmi_S_cons_za_CVOR

    !********************************************************************************************

    FUNCTION Uzmi_Z_za_GRANU(this) RESULT(grana_Z)


        CLASS(GRANA) :: this
        COMPLEX      :: grana_Z

        grana_Z = this%Z_Grane


    END FUNCTION Uzmi_Z_za_GRANU

    !********************************************************************************************

    FUNCTION Uzmi_Y_za_GRANU(this) RESULT(grana_Y)


        CLASS(GRANA) :: this
        COMPLEX      :: grana_Y

        grana_Y = this%Y_Grane


    END FUNCTION Uzmi_Y_za_GRANU

    !********************************************************************************************

    FUNCTION Get_tip_Grane(this) RESULT(tip_Gr)


        CLASS(GRANA) :: this
        INTEGER      :: tip_Gr

        tip_Gr = this%tip_Grane


    END FUNCTION Get_tip_Grane

    !********************************************************************************************
    !SET METODE:

    SUBROUTINE Dodeli_vrednost_za_P_cons_za_CVOR(this, cvor_P_cons)

        
        CLASS(CVOR)      :: this
        REAL, INTENT(IN) :: cvor_P_cons

        this%P_cons_Cvora = cvor_P_cons


    END SUBROUTINE Dodeli_vrednost_za_P_cons_za_CVOR

    !********************************************************************************************

    SUBROUTINE Dodeli_vrednost_za_Q_cons_za_CVOR(this, cvor_Q_cons)

        
        CLASS(CVOR)      :: this
        REAL, INTENT(IN) :: cvor_Q_cons

        this%Q_cons_Cvora = cvor_Q_cons


    END SUBROUTINE Dodeli_vrednost_za_Q_cons_za_CVOR

    !********************************************************************************************

    SUBROUTINE Dodeli_vrednost_za_S_cons_za_CVOR(this, cvor_S_cons)

        
        CLASS(CVOR)         :: this
        COMPLEX, INTENT(IN) :: cvor_S_cons

        this%S_cons_Cvora = cvor_S_cons


    END SUBROUTINE Dodeli_vrednost_za_S_cons_za_CVOR

    !********************************************************************************************

    SUBROUTINE Dodeli_vrednost_za_Z_za_GRANU(this, grana_Z)

        
        CLASS(GRANA)        :: this
        COMPLEX, INTENT(IN) :: grana_Z

        this%Z_Grane = grana_Z


    END SUBROUTINE Dodeli_vrednost_za_Z_za_GRANU

    !********************************************************************************************

    SUBROUTINE Dodeli_vrednost_za_Y_za_GRANU(this, grana_Y)

        
        CLASS(GRANA)        :: this
        COMPLEX, INTENT(IN) :: grana_Y

        this%Y_Grane = grana_Y


    END SUBROUTINE Dodeli_vrednost_za_Y_za_GRANU

    !********************************************************************************************


END MODULE Topologija
MODULE EES_Elementi


IMPLICIT NONE


INTEGER, PARAMETER :: ID_File = 50


!********************************************************************************************
!Broj elemenata mreze

TYPE, PUBLIC :: EES_Podaci

    PRIVATE
        INTEGER :: broj_Vodova
        INTEGER :: broj_Potrosaca
        INTEGER :: broj_Transformatora

    CONTAINS

        PROCEDURE, PUBLIC :: Ucitaj_EES           => Ucitaj_podatke_za_EES
        PROCEDURE, PUBLIC :: Get_Broj_Vodova      => Uzmi_podatak_za_broj_vodova
        PROCEDURE, PUBLIC :: Get_Broj_Potrosaca   => Uzmi_podatak_za_broj_potrosaca
        PROCEDURE, PUBLIC :: Get_Broj_Transformatora   => Uzmi_podatak_za_broj_transformatora

END TYPE EES_Podaci

PRIVATE :: Ucitaj_podatke_za_EES, Uzmi_podatak_za_broj_vodova
PRIVATE :: Uzmi_podatak_za_broj_potrosaca, Uzmi_podatak_za_broj_transformatora



!********************************************************************************************
!Korisnicki definisani tip koji modeluje ELEMENT mreze

TYPE, PUBLIC :: Element_EES
    
    PRIVATE
        INTEGER :: ID_Elementa        !sifra elementa


    CONTAINS

        PROCEDURE, PUBLIC :: Ucitaj   => Ucitaj_podatke_za_ELEMENT  !Definisana u okviru ove natklase, pa preklopljena u
                                                                    !potklasama -> princip polimorfizma "automatski radi"
        PROCEDURE, PUBLIC :: Get_ID   => Uzmi_ID_za_ELEMENT         !Definisana u natklasi   
                                                                    

END TYPE Element_EES

PRIVATE :: Ucitaj_podatke_za_ELEMENT, Uzmi_ID_za_ELEMENT


!********************************************************************************************
!Korisnicki definisani tip koji modeluje VODOVE u mrezi

TYPE, PUBLIC, EXTENDS(Element_EES) :: VOD

    PRIVATE
        REAL :: r_poduzno       !poduzna rezistansa [Om/km]
        REAL :: x_poduzno       !poduzna reaktansa [Om/km]
        REAL :: g_poduzno       !poduzna konduktansa [S/km]
        REAL :: b_poduzno       !poduzna susceptansa [S/km]
        REAL :: duzina          !duzina deonice [km]


    CONTAINS
        
        PROCEDURE, PUBLIC :: Ucitaj          => Ucitaj_podatke_za_VOD
        PROCEDURE, PUBLIC :: Get_r_poduzno   => Uzmi_r_poduzno_za_VOD
        PROCEDURE, PUBLIC :: Get_x_poduzno   => Uzmi_x_poduzno_za_VOD
        PROCEDURE, PUBLIC :: Get_g_poduzno   => Uzmi_g_poduzno_za_VOD
        PROCEDURE, PUBLIC :: Get_b_poduzno   => Uzmi_b_poduzno_za_VOD
        PROCEDURE, PUBLIC :: Get_duzina      => Uzmi_duzinu_za_VOD


END TYPE VOD

PRIVATE :: Ucitaj_podatke_za_VOD, Uzmi_r_poduzno_za_VOD, Uzmi_x_poduzno_za_VOD
PRIVATE :: Uzmi_g_poduzno_za_VOD, Uzmi_b_poduzno_za_VOD, Uzmi_duzinu_za_VOD


!********************************************************************************************
!Korisnicki definisani tip koji modeluje POTROSACE u mrezi

TYPE, PUBLIC, EXTENDS(Element_EES) :: POTROSAC

    PRIVATE
        REAL, DIMENSION(24) :: P_0      !aktivna snaga potrosnje P0 za 24 sata [??kW]
        REAL, DIMENSION(24) :: Q_0      !reaktivna snaga potrosnje Q0 za 24 sata [??kVAr]
        REAL, DIMENSION(24) :: I_0      
        REAL, DIMENSION(24) :: cos_fi_0
        REAL :: Ap
        REAL :: Bp
        REAL :: Cp
        REAL :: Aq
        REAL :: Bq
        REAL :: Cq
        REAL :: V_nom_Potr
        INTEGER :: tip_Potr
        REAL :: max_opt_Potr



    CONTAINS
        
        PROCEDURE, PUBLIC :: Ucitaj       => Ucitaj_podatke_za_POTROSAC
        PROCEDURE, PUBLIC :: Get_P_0      => Uzmi_P_0_za_POTROSAC
        PROCEDURE, PUBLIC :: Get_Q_0      => Uzmi_Q_0_za_POTROSAC
        PROCEDURE, PUBLIC :: Get_I_0
        PROCEDURE, PUBLIC :: Get_cos_fi_0
        PROCEDURE, PUBLIC :: Get_Ap
        PROCEDURE, PUBLIC :: Get_Bp
        PROCEDURE, PUBLIC :: Get_Cp
        PROCEDURE, PUBLIC :: Get_Aq
        PROCEDURE, PUBLIC :: Get_Bq
        PROCEDURE, PUBLIC :: Get_Cq
        PROCEDURE, PUBLIC :: Get_V_nom_Potr
        PROCEDURE, PUBLIC :: Get_tip_Potr
        PROCEDURE, PUBLIC :: Get_max_opt_Potr

END TYPE POTROSAC

PRIVATE :: Ucitaj_podatke_za_POTROSAC, Uzmi_P_0_za_POTROSAC
PRIVATE :: Uzmi_Q_0_za_POTROSAC

!********************************************************************************************
!Korisnicki definisani tip koji modeluje TRANSFORMATORE u mrezi

TYPE, PUBLIC, EXTENDS(Element_EES) :: TRANSFORMATOR

    PRIVATE
        REAL :: V_prim_Nom
        REAL :: V_sek_Nom
        INTEGER ::  max_Tap_Pos
        INTEGER ::  min_Tap_Pos
        REAL::  dV_Tap
        REAL::  u_k
        REAL::  S_nom
        INTEGER :: init_Tap_Pos
        
    INTEGER, PUBLIC :: tap_Pos  

    CONTAINS
        
        PROCEDURE, PUBLIC :: Ucitaj => Ucitaj_podatke_za_TRANSFORMATOR
        PROCEDURE, PUBLIC :: Get_V_prim_Nom  
        PROCEDURE, PUBLIC :: Get_V_sek_Nom   
        PROCEDURE, PUBLIC :: Get_max_Tap_Pos
        PROCEDURE, PUBLIC :: Get_min_Tap_Pos
        PROCEDURE, PUBLIC :: Get_dV_Tap
        PROCEDURE, PUBLIC :: Get_u_k
        PROCEDURE, PUBLIC :: Get_S_nom
        PROCEDURE, PUBLIC :: Get_init_Tap_Pos

END TYPE TRANSFORMATOR

PRIVATE :: Ucitaj_podatke_za_TRANSFORMATOR

!================================================================================================
!================================================================================================

CONTAINS

    !********************************************************************************************
    !Ucitavanje podataka za mrezu iz ulazne datoteke

    FUNCTION Ucitaj_podatke_za_EES(this) RESULT (Error)


        CLASS (EES_Podaci) :: this
        LOGICAL            :: Error

        Error = .false.

        
        OPEN (UNIT = ID_File, FILE='ULAZ_DATOTEKE\ULAZ_Broj_Elemenata.txt', ACTION = 'READ', STATUS = 'UNKNOWN', ERR = 100)

        
        READ (ID_File, *, ERR=101) this%broj_Vodova
        READ (ID_File, '(I4)', ERR=101) this%broj_Potrosaca
        READ (ID_File, *, ERR=101) this%broj_Transformatora

        CLOSE(ID_File)


        RETURN

        100 WRITE (*,*)  "[ERROR] Ne postoji ulazna datoteka 'Broj_Elemenata'! na odgovarajucoj lokaciji"
        Error = .true.
        STOP

        101 WRITE (*,*) "[ERROR] Greska u formatu ulazne datoteke 'Broj_Elemenata'!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_podatke_za_EES

    !********************************************************************************************

    FUNCTION Uzmi_podatak_za_broj_vodova(this) RESULT(broj_Vod)


        CLASS(EES_Podaci) :: this
        INTEGER            :: broj_Vod

        Broj_Vod = this%broj_Vodova


    END FUNCTION Uzmi_podatak_za_broj_vodova

    !********************************************************************************************

    FUNCTION Uzmi_podatak_za_broj_potrosaca(this) RESULT(broj_Potrosac)


        CLASS(EES_Podaci) :: this
        INTEGER            :: broj_Potrosac

        broj_Potrosac = this%broj_Potrosaca


    END FUNCTION Uzmi_podatak_za_broj_potrosaca

    !********************************************************************************************

    FUNCTION Uzmi_podatak_za_broj_transformatora(this) RESULT(broj_Transformator)


        CLASS (EES_Podaci) :: this
        INTEGER            :: broj_Transformator

        broj_Transformator = this%broj_Transformatora


    END FUNCTION Uzmi_podatak_za_broj_transformatora

    !********************************************************************************************

    FUNCTION Ucitaj_podatke_za_ELEMENT(this) RESULT(Error)


        CLASS (Element_EES)  :: this
        LOGICAL              :: Error

        Error = .false.

    END FUNCTION Ucitaj_podatke_za_ELEMENT

    !********************************************************************************************

    FUNCTION Uzmi_ID_za_ELEMENT(this) RESULT(ID_Element)


        CLASS (Element_EES) :: this
        INTEGER             :: ID_Element

        ID_Element = this%ID_Elementa


    END FUNCTION Uzmi_ID_za_ELEMENT

    !********************************************************************************************

    FUNCTION Ucitaj_podatke_za_VOD(this) RESULT(Error)

        CLASS (Vod)      :: this
        LOGICAL          :: Error

        Error = .false.


        READ (ID_File, 200, ERR = 101) this%ID_Elementa,                                          &
                     this%r_poduzno, this%x_poduzno, this%g_poduzno, this%b_poduzno, this%duzina

        200 FORMAT ( 5x, i9,                                          &
                      /, f10.4, 1x, f10.4, 1x, 2( e10.3, 1x ), f10.3)


        RETURN

        101 WRITE (*,*) "[ERROR] Greska u formatu ulazne datoteke 'Deonice_Podaci'!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_podatke_za_VOD

    !********************************************************************************************

    FUNCTION Uzmi_r_poduzno_za_VOD(this) RESULT(vod_r_poduzno)


        CLASS (Vod)    :: this
        REAL           :: vod_r_poduzno

        vod_r_poduzno = this%r_poduzno


    END FUNCTION Uzmi_r_poduzno_za_VOD

    !********************************************************************************************

    FUNCTION Uzmi_x_poduzno_za_VOD(this) RESULT(vod_x_poduzno)


        CLASS (Vod)    :: this
        REAL           :: vod_x_poduzno

        vod_x_poduzno = this%x_poduzno


    END FUNCTION Uzmi_x_poduzno_za_VOD

    !********************************************************************************************

    FUNCTION Uzmi_g_poduzno_za_VOD(this) RESULT(vod_g_poduzno)


        CLASS (Vod)    :: this
        REAL           :: vod_g_poduzno

        vod_g_poduzno = this%g_poduzno


    END FUNCTION Uzmi_g_poduzno_za_VOD

    !********************************************************************************************

    FUNCTION Uzmi_b_poduzno_za_VOD(this) RESULT(vod_b_poduzno)


        CLASS (Vod)    :: this
        REAL           :: vod_b_poduzno

        vod_b_poduzno = this%b_poduzno


    END FUNCTION Uzmi_b_poduzno_za_VOD

    !********************************************************************************************

    FUNCTION Uzmi_duzinu_za_VOD(this) RESULT(vod_duzina)


        CLASS (Vod)   :: this
        REAL          :: vod_duzina

        vod_duzina = this%duzina


    END FUNCTION Uzmi_duzinu_za_VOD

    !********************************************************************************************

    FUNCTION Ucitaj_podatke_za_POTROSAC(this) RESULT(Error)

        CLASS (Potrosac) :: this
        LOGICAL          :: Error
        INTEGER          :: i

        Error = .false.


        READ (ID_File, *, ERR = 101) this%ID_Elementa
        READ (ID_File, *, ERR = 101) this%V_nom_Potr, this%max_opt_Potr, this%tip_Potr   !V[V], P[MW] v I[A], PQ->10 v ICOSFI->20
        READ (ID_File, *, ERR = 101) this%Ap, this%Bp, this%Cp
        READ (ID_File, *, ERR = 101) this%Aq, this%Bq, this%Cq
        DO i=1, 24
            READ (ID_File, *, ERR = 101) this%P_0(i), this%Q_0(i), this%I_0(i), this%cos_fi_0(i)      ![r.j.]
        ENDDO

        DO i=1, 24
            this%P_0(i) = this%P_0(i) * this%max_opt_Potr
            this%Q_0(i) = this%Q_0(i) * this%max_opt_Potr
            this%I_0(i) = this%I_0(i) * this%max_opt_Potr
        ENDDO


        RETURN

        101 WRITE (*,*) "[ERROR] Greska u formatu ulazne datoteke 'Potr_Podaci'!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_podatke_za_POTROSAC

    !********************************************************************************************

    FUNCTION Uzmi_P_0_za_POTROSAC(this, hour) RESULT(potr_P_cons)


        CLASS (Potrosac)    :: this
        INTEGER, INTENT(IN) :: hour
        REAL                :: potr_P_cons

        potr_P_cons = this%P_0(hour)


    END FUNCTION Uzmi_P_0_za_POTROSAC

    !********************************************************************************************

    FUNCTION Uzmi_Q_0_za_POTROSAC(this, hour) RESULT(potr_Q_cons)


        CLASS (Potrosac)    :: this
        INTEGER, INTENT(IN) :: hour
        REAL                :: potr_Q_cons

        potr_Q_cons = this%Q_0(hour)


    END FUNCTION Uzmi_Q_0_za_POTROSAC

    !********************************************************************************************

    FUNCTION Get_I_0(this, hour) RESULT(potr_I0)


        CLASS (Potrosac)    :: this
        INTEGER, INTENT(IN) :: hour
        REAL                :: potr_I0

        potr_I0 = this%I_0(hour)


    END FUNCTION Get_I_0

    !********************************************************************************************

    FUNCTION Get_cos_fi_0(this, hour) RESULT(potr_cos_fi_0)


        CLASS (Potrosac)    :: this
        INTEGER, INTENT(IN) :: hour
        REAL                :: potr_cos_fi_0

        potr_cos_fi_0 = this%cos_fi_0(hour)


    END FUNCTION Get_cos_fi_0

    !********************************************************************************************

    FUNCTION Get_Ap(this) RESULT(potr_Ap)


        CLASS (Potrosac)  :: this
        REAL              :: potr_Ap

        potr_Ap = this%Ap


    END FUNCTION Get_Ap

    !********************************************************************************************

    FUNCTION Get_Bp(this) RESULT(potr_Bp)


        CLASS (Potrosac)  :: this
        REAL              :: potr_Bp

        potr_Bp = this%Bp


    END FUNCTION Get_Bp

    !********************************************************************************************

    FUNCTION Get_Cp(this) RESULT(potr_Cp)


        CLASS (Potrosac)  :: this
        REAL              :: potr_Cp

        potr_Cp = this%Cp


    END FUNCTION Get_Cp

    !********************************************************************************************

    FUNCTION Get_Aq(this) RESULT(potr_Aq)


        CLASS (Potrosac)  :: this
        REAL              :: potr_Aq

        potr_Aq = this%Aq


    END FUNCTION Get_Aq

    !********************************************************************************************

    FUNCTION Get_Bq(this) RESULT(potr_Bq)


        CLASS (Potrosac)  :: this
        REAL              :: potr_Bq

        potr_Bq = this%Bq


    END FUNCTION Get_Bq

    !********************************************************************************************

    FUNCTION Get_Cq(this) RESULT(potr_Cq)


        CLASS (Potrosac)  :: this
        REAL              :: potr_Cq

        potr_Cq = this%Cq


    END FUNCTION Get_Cq

    !********************************************************************************************

    FUNCTION Get_V_nom_Potr(this) RESULT(potr_V_nom_Potr)


        CLASS (Potrosac)  :: this
        REAL              :: potr_V_nom_Potr

        potr_V_nom_Potr = this%V_nom_Potr


    END FUNCTION Get_V_nom_Potr

    !********************************************************************************************

    FUNCTION Get_tip_Potr(this) RESULT(potr_tip_Potr)


        CLASS (Potrosac)  :: this
        INTEGER           :: potr_tip_Potr

        potr_tip_Potr = this%tip_Potr


    END FUNCTION Get_tip_Potr

    !********************************************************************************************

    FUNCTION Get_max_opt_Potr(this) RESULT(max_opt)


        CLASS (Potrosac)  :: this
        REAL              :: max_opt

        max_opt = this%max_opt_Potr


    END FUNCTION Get_max_opt_Potr

    !********************************************************************************************

    FUNCTION Ucitaj_podatke_za_TRANSFORMATOR(this) RESULT(Error)

        CLASS (Transformator)      :: this
        LOGICAL                    :: Error

        Error = .false.


        READ (ID_File, *, ERR = 101) this%ID_Elementa, this%V_prim_Nom, this%V_sek_Nom, this%max_Tap_Pos, &
                                     this%min_Tap_Pos, this%dV_Tap, this%u_k, this%S_nom, this%init_Tap_Pos

        
        RETURN

        101 WRITE (*,*) "[ERROR] Greska u formatu ulazne datoteke 'Transformatori_Podaci'!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_podatke_za_TRANSFORMATOR

    !********************************************************************************************

    FUNCTION Get_V_prim_Nom(this) RESULT(V_prim_N)


        CLASS (TRANSFORMATOR)  :: this
        REAL                   :: V_prim_N

        V_prim_N = this%V_prim_Nom


    END FUNCTION Get_V_prim_Nom

    !********************************************************************************************

    FUNCTION Get_V_sek_Nom(this) RESULT(V_sek_N)


        CLASS (TRANSFORMATOR)  :: this
        REAL                   :: V_sek_N

        V_sek_N = this%V_sek_Nom


    END FUNCTION Get_V_sek_Nom

    !********************************************************************************************

    FUNCTION Get_max_Tap_Pos(this) RESULT(max_Tap_P)


        CLASS (TRANSFORMATOR)  :: this
        INTEGER                :: max_Tap_P

        max_Tap_P = this%max_Tap_Pos


    END FUNCTION Get_max_Tap_Pos

    !********************************************************************************************

    FUNCTION Get_min_Tap_Pos(this) RESULT(min_Tap_P)


        CLASS (TRANSFORMATOR)  :: this
        INTEGER                :: min_Tap_P

        min_Tap_P = this%min_Tap_Pos


    END FUNCTION Get_min_Tap_Pos

    !********************************************************************************************

    FUNCTION Get_dV_Tap(this) RESULT(dV_T)


        CLASS (TRANSFORMATOR)  :: this
        REAL                   :: dV_T

        dV_T = this%dV_Tap


    END FUNCTION Get_dV_Tap

    !********************************************************************************************

    FUNCTION Get_u_k(this) RESULT(uk)


        CLASS (TRANSFORMATOR)  :: this
        REAL                   :: uk

        uk = this%u_k


    END FUNCTION Get_u_k

    !********************************************************************************************

    FUNCTION Get_S_nom(this) RESULT(S_n)


        CLASS (TRANSFORMATOR)  :: this
        REAL                   :: S_n

        S_n = this%S_nom


    END FUNCTION Get_S_nom

    !********************************************************************************************

    FUNCTION Get_init_Tap_Pos(this) RESULT(init_Tap_P)


        CLASS (TRANSFORMATOR)  :: this
        INTEGER                :: init_Tap_P

        init_Tap_P = this%init_Tap_Pos


    END FUNCTION Get_init_Tap_Pos

    !********************************************************************************************

END MODULE EES_Elementi
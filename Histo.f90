module Histo

  implicit none

contains

Real Function sortie(Tab, taille_histo, inf, sup)

    Real, dimension(1:taille_histo) :: Histogramme
    Real, dimension(:), intent(in) ::Tab
    Real, intent(in) :: inf, sup
    Integer, intent(in) :: taille_histo
    Real, dimension(1:taille_histo) :: Bornes_histo
    Real :: ecart
    integer :: i, j

    ecart = (sup-inf)/(taille_histo*1.)

    do i=1,taille_histo
       Histogramme(i) = 0
       Bornes_histo(i) = inf + (i-1) * ecart
       write(6,*) Bornes_histo(i)
    end do

    do i=1, size(Tab)
       if (Tab(i) > Bornes_histo(taille_histo)) then
          Histogramme(taille_histo) = Histogramme(taille_histo) + 1
       else
          do j=2, taille_histo
             if (Tab(i) < Bornes_histo(j)) then
                Histogramme(j-1) = Histogramme(j-1) + 1
                exit
            end if
          end do
       end if
    end do
    Open(1,file='Histogramme.dat')
    do i = 1,size(Histogramme)
        write(1,*) Histogramme(i)
    end do
    Close(1)

    sortie = 1.
  end Function sortie


End module Histo

module Histogramme

  implicit none

contains

  Function Histogramme(Tab, taille_histo, inf, sup)

    Real, dimension(1:taille_histo) :: Histogramme
    Real, dimension(:), intent(in) ::Tab
    Real, intent(in) :: inf, sup
    Integer, intent(in) :: taille_histo
    Real, dimension(1:taille_histo) :: Bornes_histo
    Real :: ecart
    integer :: i, j

    ecart = (sup-inf)/(taille_histo*1.)

    do i=1,taille_histo
       Histogramme[i] = 0
       Bornes_histo[i] = inf + (i-1) * ecart
    end do

    do i=1, size(Tab)
       if (Tab[i] < inf) then
          Histogramme[1] = Histogramme[1] + 1
       elseif (Tab[i] > sup) then
          Histogramme[taille_histo] = Histogramme[taille_histo] + 1
       else
          do j=2, taille_histo-1
             if (Tab[i] < Bornes_histo[j]) then
                Histogramme[j] = Histogramme[j] + 1
             exit
         
          end do
       end if
    end do
  end Function Histogramme


End module Histogramme

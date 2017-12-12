module Histo

  implicit none

contains

Real Function sortie(Tab, taille_histo, sup)

    Integer, dimension(1:taille_histo) :: Histogramme
    Real, dimension(:), intent(in) :: Tab
    Real, intent(in) :: sup
    Integer, intent(in) :: taille_histo
    integer :: i, k
    Real :: N

    N = taille_histo/sup

    Do i = 1, taille_histo
      Histogramme(i) = 0
    End Do

    Do i = 1, size(Tab)
      k = floor(Tab(i)*N)
      if (k > taille_histo) then
        k = taille_histo
      elseif (k < 1) then
        k = 1
      end if
      !write(6,*) i, k, Tab(i)
      Histogramme(k) = Histogramme(k) + 1
    End Do

    Open(1,file='Histogramme.dat')
    do i = 1,size(Histogramme)
        write(1,*) Histogramme(i)
    end do
    Close(1)

    sortie = 1.
  end Function sortie


End module Histo

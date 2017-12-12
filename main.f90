Program Main

  Use Data
  Use functions
  Use Evolution_Energie
  Use Histo

  implicit none

  Integer, parameter :: N = 10000
  Integer :: Niter, i
  Real, dimension(N) :: Energies
  Real :: verif_sortie

  Call ReadData()
  Open(1,file='Temperature.dat')
  ! Open(2,file='Energies.dat')


  Energies = NRJ_Aleatoires(N)
  Niter = ceiling(tau/dt)
  !write(6,*) Energies
  Do i = 1, Niter
    !write(6,*) "******* Itération : ", i
    T = Temperature(Energies, R)
    Energies = Evol(Energies)
    write(1,*) T
  End Do
  T = Temperature(Energies, R)
  write(1,*) T
  ! Do i = 1, size(Energies)
  !   write(2,*) Energies(i)
  ! End Do
  !write(6,*) "Boucle terminée"
  verif_sortie = sortie(Energies,100,1000000.)
  !write(6,*) "Histogramme écrit"

  Close(1)
  ! Close(2)


  !write(6,*) Vecteur

End Program Main

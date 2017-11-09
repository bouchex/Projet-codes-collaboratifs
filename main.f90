Program Main

  Use Data
  Use functions
  Use Evolution_Energie
  Use Histo

  implicit none

  Integer, parameter :: N = 10
  Integer :: Niter, i
  Real, dimension(N) :: Energies, Vect_sigma
  Real :: verif_sortie

  Call ReadData()
  Open(1,file='Temperature.dat')


  Energies = NRJ_Aleatoires(N)
  Vect_sigma = Vect_Aleatoire(N)
  Niter = ceiling(tau/dt)
  write(6,*) Energies
  Do i = 1, Niter
    T = Temperature(Energies, R)
    Energies = Evol(Energies,Vect_sigma)
    write(1,*) T
  End Do
  T = Temperature(Energies, R)
  verif_sortie = sortie(Energies,10,100000.,200000.)

  Close(1)


  !write(6,*) Vecteur

End Program Main

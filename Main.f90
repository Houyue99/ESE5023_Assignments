program Main 
implicit none

integer:: u,v,w,i,j,a,b,c

real(4),dimension(3,3)  ::  M,N,MN

u=50
v=60
w=70
a=3
b=3
c=3
open(unit=u,file='/work/ese-ouycc/fortran_2/M.dat',status='old')
open(unit=v,file='/work/ese-ouycc/fortran_2/N.dat',status='old')

j=1
do i =1,3
read(u,*)M(i,j),M(i,j+1),M(i,j+2)
read(v,*)N(i,j),N(i,j+1),N(i,j+2)
enddo

close(u)
close(v)

call Matrix_multip(M,N,MN,a,b,c)
open(unit=w,file='MN.dat',status='replace')

do i=1,3
write(w,'(f8.1,f8.1,f8.1)') MN(i,j),MN(i,j+1),MN(i,j+2)
enddo
close(w)


end program Main

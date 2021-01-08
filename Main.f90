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

! MingYANG noticed:
! (1) in this question, M and N are in 4×3 and 3×3, so the result should be 4×3
! (2) using "matmul" can`t get the right answer
! for M=[9.4889891 15.79952 9.2889578
!        9.2889578 12.92396 5.8621211
!        5.8621211 11.29471 14.04269
!        1.9356927 18.60917 18.23201]
!     N=[7.7234138 14.11560 1.4449604
!        5.5518050 14.80624 14.04269
!        0.5965542 18.58036 2.2660391]
!     MN=M*N= [166.5446  540.4664  256.6281
!              146.9908  431.3948  208.1931
!              116.3588  510.8978  198.8999
!              129.1410  641.6126  305.4343]
! the end

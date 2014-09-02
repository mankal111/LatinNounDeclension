program latinnoundeclension
implicit none
integer:: seed,i,j,k,l,n,w,nq,ncq
character(18),allocatable,dimension(:,:):: nounsarr
character(18):: ans
integer*4 today(3), now(3)


open(10,file='nouns.txt')
read(10,*) n
allocate (nounsarr(n*6,2))
seed=7654321+2*int(secnds(0.0))
i= int(rand(seed))

do i=1,((n)*6)
read(10,*) nounsarr(i,1),nounsarr(i,2)
end do
nq=-1
ncq=0
print*,'Type quit whenever you want to stop.'
do
nq=nq+1
w=int(rand()*n+1)
i=int(rand()*6+1)
j=int(rand()*2+1)
k=int(rand()*6+1)
l=int(rand()*2+1)

print*,'Type the ',trim(text(i,1)),' ',trim(text(j,2))&
	,' of the noun ',nounsarr((w-1)*6+k,l)
read*, ans
if (ans=='quit') exit

if (ans==nounsarr((w-1)*6+i,j)) then
print*,'Right!'
ncq=ncq+1
else
print*,'Wrong... The right answer is ',trim(nounsarr((w-1)*6+i,j))
end if
end do

write (*, 2000) ncq,nq,(ncq*100/nq)
open(20,file='nounsrecords.txt', POSITION='APPEND')
call idate(today)   ! today(1)=day, (2)=month, (3)=year
call itime(now)     ! now(1)=hour, (2)=minute, (3)=second
write (20, 1000 )  today(2), today(1), today(3), now,ncq,nq,(ncq*100/nq)
 1000 format ( i2.2,'/',i2.2,'/',i4.4,' ',i2.2,':',i2.2,':'&
	,i2.2,'	',i3,'	',i3,'		' , i3.2,'%')
 2000 format ('Right answers', i3, ' out of ', i3,'. 	Percentage:', i3.2, '%')

close(10)
close(20)
contains
function text(fi,sel)
integer::fi,sel
character(30):: text

if (sel==1) then
select case (fi)
case (1)
text='nominative'
case (2)
text='genitive'
case (3)
text='dative'
case (4)
text='accusative'
case (5)
text='ablative'
case (6)
text='vocative'
end select
else
select case(fi)
case (1)
text='singular'
case (2)
text='plural'
end select
end if
end function

end program nouns

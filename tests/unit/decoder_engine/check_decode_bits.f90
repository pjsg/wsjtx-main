! Checks the message bits in jt9's <DecodeEvidence> lines: each bit string
! must unpack to the message on the decode line that follows it.
!
! Usage: check_decode_bits BITS FILE
!   BITS  72 (JT4, JT9, JT65), 77 (FT8, FT4, Q65, MSK144, FST4) or 50 (FST4W)
!   FILE  jt9 output written with --evidence
program check_decode_bits
  use packjt, only: unpackmsg
  use packjt77, only: unpack77
  implicit none
  character(len=512) :: arg,path,line,evidence
  character(len=37) :: message
  character(len=77) :: c77
  character(len=:), allocatable :: hex
  integer :: nbits,unit,ios,compared,skipped,i,p
  integer :: dat(12)
  logical :: ok,pending

  call get_command_argument(1,arg)
  read(arg,*) nbits
  call get_command_argument(2,path)
  open(newunit=unit,file=trim(path),status='old',action='read')

  compared=0
  skipped=0
  pending=.false.
  do
     read(unit,'(a)',iostat=ios) line
     if(ios/=0) exit
     if(index(line,'<DecodeEvidence> ')==1) then
        p=index(line,' bits=')
        if(p==0) error stop 'evidence line without bits'
        evidence=line
        hex=trim(line(p+6:))
        pending=hex/='none'
        cycle
     endif
     if(.not.pending) cycle
     pending=.false.
     if(len(hex)/=2*((nbits+7)/8)) then
        write(*,'(3a)') 'wrong length: ',trim(evidence)
        error stop 1
     endif
     c77=repeat('0',77)
     do i=1,nbits
        c77(i:i)=merge('1','0',btest(nibble(hex((i-1)/4+1:(i-1)/4+1)),3-mod(i-1,4)))
     enddo
     message=' '
     if(nbits==72) then
        do i=1,12
           read(c77(6*i-5:6*i),'(b6)') dat(i)
        enddo
        call unpackmsg(dat,message(1:22))
        ok=.true.
     else
        ! FST4W's 50 bits are a type 0.6 message once padded as its decoder does.
        if(nbits==50) c77(51:77)='000000000000000000000110000'
        call unpack77(c77,1,message,ok)
     endif
     if(.not.ok) then
        write(*,'(3a)') 'bits do not unpack: ',trim(evidence)
        error stop 2
     endif
     ! A fresh unpacker cannot resolve a hashed call the decoder had seen.
     if(index(message,'<')>0) then
        skipped=skipped+1
        cycle
     endif
     if(index(line,trim(message))==0) then
        write(*,'(5a)') 'bits unpack to "',trim(message),'", not the decode: ',trim(line)
        error stop 3
     endif
     compared=compared+1
  enddo
  close(unit)
  write(*,'(a,i0,a,i0,a)') 'compared ',compared,' decodes (',skipped,' with hashed calls skipped)'
  if(compared==0) error stop 'no decodes with bits'

contains

  integer function nibble(c)
    character(len=1), intent(in) :: c
    nibble=index('0123456789ABCDEF',c)-1
    if(nibble<0) error stop 'bad hex digit'
  end function nibble

end program check_decode_bits

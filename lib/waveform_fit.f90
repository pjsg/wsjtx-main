module waveform_fit
! Refines the start time and frequency of a decoded FT8 or FT4 signal by
! fitting its reconstructed reference waveform to the 12 kHz audio.
!
! The decoders find a signal on coarse grids (FT8: 5 ms and 0.5 Hz; FT4:
! 1.5 ms and 1 Hz). Once the message is known, its exact waveform can be
! correlated against the full-rate audio:
!
!   z_k(n,f) = sum over segment k of dd(n+i) * conj(cref(i)) * exp(-j2pi f (i-1)/fs)
!   M(n,f)   = sum_k |z_k(n,f)|**2
!
! Segments are coherent over a few symbols and combined in power, so slow
! fading and phase wander do not destroy the metric. The start is the vertex
! of a least-squares quadratic fitted to M across offsets around its peak; the
! frequency correction is the phase advance from segment to segment.
  implicit none
  private
  public :: refine_waveform_fit,refine_ft8_waveform,refine_ft4_waveform

  ! The reference with the trial frequency applied, conjugated.
  type :: reference
     real, allocatable :: re(:),im(:)
  end type reference

  real(8), parameter :: TWOPI=8.0d0*atan(1.0d0)
  real(8), parameter :: FS=12000.0d0
  integer, parameter :: NPOINTS=8           ! fit points either side of the centre
  integer, parameter :: NARROWING(2)=[1,4]  ! final fit windows: fractions of the first
  integer, parameter :: NFREQS=1            ! coarse frequencies either side, fs/(4 nseg) apart

contains

  ! FT8: start is the waveform start (first symbol) in seconds from dd(1);
  ! freq is tone 0 in Hz. Segments are 2 symbols (0.32 s).
  subroutine refine_ft8_waveform(dd,itone,start,freq,refined)
    use ft8var_waveform_generator, only: gen_ft8wavevar_samples
    real, intent(in) :: dd(:)
    integer, intent(in) :: itone(79)
    real, intent(inout) :: start,freq
    logical, intent(out) :: refined
    integer, parameter :: NSPS=1920,NWAVE=79*NSPS
    complex, allocatable :: cref0(:)

    allocate(cref0(NWAVE))
    call gen_ft8wavevar_samples(itone,79,NSPS,2.0,12000.0,0.0,NWAVE,1,NWAVE,cwave=cref0)
    call refine_waveform_fit(dd,cref0,2*NSPS,NSPS/8,start,freq,refined)
  end subroutine refine_ft8_waveform

  ! FT4: start is the reference waveform start, one ramp symbol before the
  ! first message symbol, in seconds from dd(1); freq is tone 0 in Hz.
  ! Segments are 8 symbols (0.384 s).
  subroutine refine_ft4_waveform(dd,itone,start,freq,refined)
    real, intent(in) :: dd(:)
    integer, intent(in) :: itone(103)
    real, intent(inout) :: start,freq
    logical, intent(out) :: refined
    integer, parameter :: NSPS=576,NWAVE=(103+2)*NSPS
    complex, allocatable :: cref0(:)
    real, allocatable :: unused(:)

    allocate(cref0(NWAVE),unused(NWAVE))
    call gen_ft4wave(itone,103,NSPS,12000.0,0.0,cref0,unused,1,NWAVE)
    call refine_waveform_fit(dd,cref0,8*NSPS,NSPS/8,start,freq,refined)
  end subroutine refine_ft4_waveform

  ! dd       : audio, 12 kHz, dd(1) at time zero
  ! cref0    : complex reference waveform at 0 Hz, cref0(1) at the waveform start
  ! nseg     : samples per coherent segment
  ! nhalf    : half-width, in samples, of the window the quadratic is fitted over
  ! start    : in, coarse waveform start (s); out, refined start
  ! freq     : in, coarse frequency of tone 0 (Hz); out, refined frequency
  ! refined  : out, true when both were refined; otherwise start and freq are
  !            returned unchanged
  subroutine refine_waveform_fit(dd,cref0,nseg,nhalf,start,freq,refined)
    real, intent(in) :: dd(:)
    complex, intent(in) :: cref0(:)
    integer, intent(in) :: nseg,nhalf
    real, intent(inout) :: start,freq
    logical, intent(out) :: refined
    type(reference) :: cr
    real(8) :: centre,f,vertex,df,best,m,fbest
    integer :: iter,istep,k,j
    logical :: ok

    refined=.false.
    istep=max(1,nhalf/NPOINTS)
    allocate(cr%re(size(cref0)),cr%im(size(cref0)))
    centre=start*FS
    f=freq

    ! Coarse: the best point on a grid of starts and of frequencies a quarter
    ! of fs/nseg apart about the decoder's estimate.
    best=-1.0d0
    vertex=centre
    fbest=f
    do j=-NFREQS,NFREQS
       call mix(cref0,f+j*0.25d0*FS/nseg,cr)
       do k=-NPOINTS,NPOINTS
          m=metric(dd,cr,nseg,nint(centre)+k*istep)
          if(m.gt.best) then
             best=m
             vertex=nint(centre)+k*istep
             fbest=f+j*0.25d0*FS/nseg
          endif
       enddo
    enddo
    if(best.le.0.0d0) return
    centre=vertex
    f=fbest
    call mix(cref0,f,cr)

    do iter=1,3
       call fit_start(dd,cr,nseg,istep,centre,vertex,ok)
       if(.not.ok) return
       centre=vertex
       df=frequency_offset(dd,cr,nseg,nint(centre))
       if(abs(df).gt.0.5d0*FS/nseg) return     ! beyond the unambiguous range
       f=f+df
       call mix(cref0,f,cr)
       if(abs(df).lt.1.0d-3 .and. iter.gt.1) exit
    enddo
    ! Near its peak the metric is closer to quadratic, so finish with
    ! narrower windows; the wide window above only has to get close.
    do k=1,size(NARROWING)
       call fit_start(dd,cr,nseg,max(1,istep/NARROWING(k)),centre,vertex,ok)
       if(.not.ok) return
       centre=vertex
    enddo

    ! A signal or path nearby can pull the fit. The decoder's estimate is
    ! within about 1 Hz, so a larger move means the fit has gone astray.
    ! (A starting error near a multiple of fs/nseg, 3.1 Hz for FT8 and 2.6 Hz
    ! for FT4, would alias undetected; the decoders' errors are far smaller.)
    if(abs(f-freq).gt.0.5d0*FS/nseg) return

    start=real(vertex/FS)
    freq=real(f)
    refined=.true.
  end subroutine refine_waveform_fit

  ! cr(i) = conj(cref0(i)) * exp(-j 2pi f (i-1)/fs)
  subroutine mix(cref0,f,cr)
    complex, intent(in) :: cref0(:)
    real(8), intent(in) :: f
    type(reference), intent(inout) :: cr
    complex(8) :: w,step,c
    integer :: i

    step=exp(cmplx(0.0d0,-TWOPI*f/FS,kind=8))
    w=(1.0d0,0.0d0)
    do i=1,size(cref0)
       c=conjg(cmplx(cref0(i),kind=8))*w
       cr%re(i)=real(real(c),kind(cr%re))
       cr%im(i)=real(aimag(c),kind(cr%im))
       w=w*step
       if(mod(i,4096).eq.0) w=exp(cmplx(0.0d0,-TWOPI*f*i/FS,kind=8))
    enddo
  end subroutine mix

  ! Segment correlations for the reference starting at audio sample n+1.
  subroutine correlate(dd,cr,nseg,n,z,nz)
    real, intent(in) :: dd(:)
    type(reference), intent(in) :: cr
    integer, intent(in) :: nseg,n
    complex(8), intent(out) :: z(:)
    integer, intent(out) :: nz
    integer :: i0,i1,k,ndd
    real :: zr,zi

    ndd=size(dd)
    nz=size(cr%re)/nseg
    do k=1,nz
       i0=max((k-1)*nseg+1,1-n)
       i1=min(k*nseg,ndd-n)
       call segment_sum(dd(n+i0:),cr%re(i0:),cr%im(i0:),i1-i0+1,zr,zi)
       z(k)=cmplx(zr,zi,kind=8)
    enddo
  end subroutine correlate

  ! zr + j zi = sum of x(i)*(a(i) + j b(i)), i=1..n. Four independent
  ! partial sums keep the floating-point adds from serialising.
  subroutine segment_sum(x,a,b,n,zr,zi)
    integer, intent(in) :: n
    real, intent(in) :: x(*),a(*),b(*)
    real, intent(out) :: zr,zi
    real :: r1,r2,r3,r4,i1,i2,i3,i4
    integer :: i,m

    r1=0; r2=0; r3=0; r4=0; i1=0; i2=0; i3=0; i4=0
    m=n-mod(max(n,0),4)
    do i=1,m,4
       r1=r1+x(i)*a(i)
       r2=r2+x(i+1)*a(i+1)
       r3=r3+x(i+2)*a(i+2)
       r4=r4+x(i+3)*a(i+3)
       i1=i1+x(i)*b(i)
       i2=i2+x(i+1)*b(i+1)
       i3=i3+x(i+2)*b(i+2)
       i4=i4+x(i+3)*b(i+3)
    enddo
    do i=m+1,n
       r1=r1+x(i)*a(i)
       i1=i1+x(i)*b(i)
    enddo
    zr=(r1+r2)+(r3+r4)
    zi=(i1+i2)+(i3+i4)
  end subroutine segment_sum

  real(8) function metric(dd,cr,nseg,n)
    real, intent(in) :: dd(:)
    type(reference), intent(in) :: cr
    integer, intent(in) :: nseg,n
    complex(8) :: z(size(cr%re)/nseg)
    integer :: nz

    call correlate(dd,cr,nseg,n,z,nz)
    metric=sum(real(z(1:nz))**2+aimag(z(1:nz))**2)
  end function metric

  ! Frequency error from the mean phase advance between adjacent segments.
  real(8) function frequency_offset(dd,cr,nseg,n)
    real, intent(in) :: dd(:)
    type(reference), intent(in) :: cr
    integer, intent(in) :: nseg,n
    complex(8) :: z(size(cr%re)/nseg),p
    integer :: nz,k

    call correlate(dd,cr,nseg,n,z,nz)
    p=(0.0d0,0.0d0)
    do k=1,nz-1
       p=p+z(k+1)*conjg(z(k))
    enddo
    frequency_offset=0.0d0
    if(abs(p).gt.0.0d0) frequency_offset=atan2(aimag(p),real(p))*FS/(TWOPI*nseg)
  end function frequency_offset

  ! Fits M = a + b x + c x**2 over x = -NPOINTS..NPOINTS steps of istep
  ! samples about centre; vertex is the start, in samples, at the fitted peak.
  subroutine fit_start(dd,cr,nseg,istep,centre,vertex,ok)
    real, intent(in) :: dd(:)
    type(reference), intent(in) :: cr
    integer, intent(in) :: nseg,istep
    real(8), intent(in) :: centre
    real(8), intent(out) :: vertex
    logical, intent(out) :: ok
    real(8) :: y,x,sy,sxy,sx2y,sx2,sx4,b,c,n0
    integer :: k,ic

    ic=nint(centre)
    sy=0; sxy=0; sx2y=0; sx2=0; sx4=0
    do k=-NPOINTS,NPOINTS
       y=metric(dd,cr,nseg,ic+k*istep)
       x=k
       sy=sy+y
       sxy=sxy+x*y
       sx2y=sx2y+x*x*y
       sx2=sx2+x*x
       sx4=sx4+x**4
    enddo
    n0=2*NPOINTS+1
    b=sxy/sx2
    c=(sx2y-sx2*sy/n0)/(sx4-sx2*sx2/n0)
    ok=c.lt.0.0d0
    vertex=centre
    if(.not.ok) return
    x=-b/(2.0d0*c)
    ok=abs(x).le.NPOINTS
    if(ok) vertex=ic+x*istep
  end subroutine fit_start

end module waveform_fit

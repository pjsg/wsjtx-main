! Precision of the start and frequency refinement that FT8 and FT4 decodes
! carry as evidence (lib/waveform_fit.f90), on synthetic signals at
! fractional-sample starts, with and without noise.
program test_waveform_fit
  use waveform_fit, only: refine_ft8_waveform,refine_ft4_waveform
  use ft8var_waveform_generator, only: gen_ft8wavevar_samples
  use ft4_codec, only: ft4_tones_from_77bits
  implicit none
  integer, parameter :: NMAX=180000
  integer(8) :: seed=20261109_8
  integer :: last_tones8(79),last_tones4(103)

  ! mode, SNR (dB in 2500 Hz), trials, RMS start limit (ms), RMS frequency limit (Hz)
  call check(8,99.0,6,0.05,0.001)
  call check(8,-15.0,16,1.0,0.02)
  call check(4,99.0,6,0.05,0.001)
  call check(4,-12.0,16,1.0,0.03)
  call check_frequency_range()
  call check_gives_up()
  write(*,'(a)') 'PASS'

contains

  subroutine check(mode,snr,trials,start_limit_ms,freq_limit_hz)
    integer, intent(in) :: mode,trials
    real, intent(in) :: snr,start_limit_ms,freq_limit_hz
    real, allocatable :: dd(:)
    real :: start,freq,t0,f0,se2,sf2
    integer :: trial
    logical :: refined

    allocate(dd(NMAX))
    se2=0.0
    sf2=0.0
    do trial=1,trials
       f0=500.0+2000.0*uniform()
       ! A fractional-sample start, 0.3 to 0.7 s into the period
       t0=(3600.0+4800.0*uniform()+uniform())/12000.0
       call synthesize(mode,t0,f0,snr,dd)
       ! Start from errors as large as the decoders' grids leave
       start=t0+(uniform()-0.5)*merge(0.006,0.003,mode==8)
       freq=f0+(uniform()-0.5)*merge(0.5,1.0,mode==8)
       if(mode==8) then
          call refine_ft8_waveform(dd,last_tones8,start,freq,refined)
       else
          call refine_ft4_waveform(dd,last_tones4,start,freq,refined)
       endif
       if(.not.refined) then
          write(*,*) 'mode',mode,'SNR',snr,'trial',trial,': not refined'
          error stop 1
       endif
       se2=se2+((start-t0)*1000.0)**2
       sf2=sf2+(freq-f0)**2
    enddo
    write(*,'(a,i2,a,f6.1,a,f8.4,a,f8.5)') 'mode',mode,'  SNR',snr, &
         '  RMS start error (ms)',sqrt(se2/trials),'  RMS frequency error (Hz)',sqrt(sf2/trials)
    if(sqrt(se2/trials).gt.start_limit_ms) error stop 2
    if(sqrt(sf2/trials).gt.freq_limit_hz) error stop 3
  end subroutine check

  ! A starting frequency 1.2 Hz out, beyond the decoders' error, still
  ! converges.
  subroutine check_frequency_range()
    real, allocatable :: dd(:)
    real :: start,freq
    logical :: refined

    allocate(dd(NMAX))
    call synthesize(8,0.5,1500.0,99.0,dd)
    start=0.5
    freq=1501.2
    call refine_ft8_waveform(dd,last_tones8,start,freq,refined)
    write(*,'(a,f9.5,a,f10.5)') '1.2 Hz out: start error (ms)',(start-0.5)*1000.0,'  frequency',freq
    if(.not.refined .or. abs(start-0.5).gt.0.0001 .or. abs(freq-1500.0).gt.0.001) error stop 4
  end subroutine check_frequency_range

  ! With no signal there is nothing to fit: the estimates are left unchanged.
  subroutine check_gives_up()
    real, allocatable :: dd(:)
    real :: start,freq
    logical :: refined

    allocate(dd(NMAX))
    call synthesize(8,0.5,1500.0,99.0,dd)
    dd=0.0
    start=0.5
    freq=1500.0
    call refine_ft8_waveform(dd,last_tones8,start,freq,refined)
    if(refined .or. start.ne.0.5 .or. freq.ne.1500.0) error stop 5
  end subroutine check_gives_up

  ! dd = noise + a random message's waveform starting t0 seconds after dd(1)
  subroutine synthesize(mode,t0,f0,snr,dd)
    integer, intent(in) :: mode
    real, intent(in) :: t0,f0,snr
    real, intent(out) :: dd(:)
    integer(1) :: bits(77)
    real, allocatable :: wave(:)
    complex, allocatable :: unused(:)
    real :: sigma
    integer :: j,nwave

    do j=1,77
       bits(j)=merge(1_1,0_1,uniform().gt.0.5)
    enddo
    if(mode==8) then
       nwave=79*1920
       allocate(wave(nwave))
       call get_ft8_tones_from_77bits(bits,last_tones8)
       call gen_ft8wavevar_samples(last_tones8,79,1920,2.0,12000.0,f0,nwave,1,nwave,wave=wave)
    else
       nwave=105*576
       allocate(wave(nwave),unused(nwave))
       call ft4_tones_from_77bits(bits,last_tones4)
       call gen_ft4wave(last_tones4,103,576,12000.0,f0,unused,wave,0,nwave)
    endif
    ! Unit-amplitude sine: power 1/2 against noise in 2500 Hz of the 6 kHz band
    sigma=sqrt(0.5/(10.0**(snr/10.0))*6000.0/2500.0)
    do j=1,size(dd)
       dd(j)=sigma*gaussian()+interpolate(wave,(j-1)-t0*12000.0+1.0)
    enddo
  end subroutine synthesize

  ! Windowed-sinc value of x at fractional index t, x(1) at t=1
  real function interpolate(x,t)
    real, intent(in) :: x(:),t
    real, parameter :: PI=3.14159265
    integer :: k,k0
    real :: d,s

    interpolate=0.0
    k0=floor(t)
    if(k0.lt.-32 .or. k0.gt.size(x)+32) return
    do k=max(1,k0-31),min(size(x),k0+32)
       d=t-k
       s=1.0
       if(abs(d).gt.1.0e-6) s=sin(PI*d)/(PI*d)
       interpolate=interpolate+x(k)*s*(0.5+0.5*cos(PI*d/32.0))
    enddo
  end function interpolate

  ! xorshift64: deterministic across compilers, no integer overflow
  real function uniform()
    seed=ieor(seed,ishft(seed,13))
    seed=ieor(seed,ishft(seed,-7))
    seed=ieor(seed,ishft(seed,17))
    uniform=real(ibits(seed,20,24))/2.0**24
  end function uniform

  real function gaussian()
    real :: u1,u2
    u1=max(uniform(),1.0e-7)
    u2=uniform()
    gaussian=sqrt(-2.0*log(u1))*cos(6.2831853*u2)
  end function gaussian

end program test_waveform_fit

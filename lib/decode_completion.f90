module decode_completion_module
  use iso_fortran_env, only: int64
  use decoder_ipc_atomic, only: decoder_ipc_progress_report
  implicit none

  private
  public :: decode_completion_result
  public :: reset_decode_completion
  public :: set_decode_completion
  public :: write_decode_completion
  public :: write_decode_progress
  public :: enable_decode_evidence_records
  public :: write_decode_evidence_record

  ! Under the GUI, jt9 precedes an FT8 or FT4 decode line with what the
  ! decoder measured: message start, precise frequency, and message bits.
  ! Standalone jt9 output is left unchanged.
  logical, save :: evidence_records_enabled = .false.

  type :: decode_completion_result
     logical :: available = .false.
     integer :: synchronized = 0
     integer :: decoded = 0
     integer :: average = 0
  end type decode_completion_result

  integer, save :: progress_thread_generation = 0
  integer, save :: progress_thread_calls = 0
  integer(int64), save :: progress_thread_count = 0_int64
  integer(int64), save :: progress_thread_rate = 0_int64
!$omp threadprivate(progress_thread_generation, progress_thread_calls, &
!$omp& progress_thread_count, progress_thread_rate)

contains

  subroutine reset_decode_completion(completion)
    type(decode_completion_result), intent(out) :: completion

    completion%available = .false.
    completion%synchronized = 0
    completion%decoded = 0
    completion%average = 0
  end subroutine reset_decode_completion

  subroutine set_decode_completion(completion, synchronized, decoded, average)
    type(decode_completion_result), intent(out) :: completion
    integer, intent(in) :: synchronized, decoded, average

    completion%available = .true.
    completion%synchronized = synchronized
    completion%decoded = decoded
    completion%average = average
  end subroutine set_decode_completion

  subroutine write_decode_completion(completion, generation)
    type(decode_completion_result), intent(in) :: completion
    integer, intent(in), optional :: generation

    if (.not. completion%available) return
    if (present(generation)) then
       write(*,1001) completion%synchronized, completion%decoded, &
            completion%average, generation
1001   format('<DecodeFinished>',2i4,i9,' gen=',i0)
    else
       write(*,1000) completion%synchronized, completion%decoded, &
            completion%average
1000   format('<DecodeFinished>',2i4,i9)
    end if
  end subroutine write_decode_completion

  subroutine write_decode_progress(generation)
    integer, intent(in) :: generation
    integer(int64) :: count, count_max, elapsed, rate

    if (generation <= 0) return
    if (generation == progress_thread_generation) then
       progress_thread_calls = progress_thread_calls + 1
       if (progress_thread_calls < 32) return
    end if
    progress_thread_calls = 0
    call system_clock(count, rate, count_max)
    if (generation == progress_thread_generation .and. &
         rate == progress_thread_rate .and. rate > 0_int64) then
       if (count >= progress_thread_count) then
          elapsed = count - progress_thread_count
       else
          elapsed = count_max - progress_thread_count + count + 1_int64
       end if
       if (elapsed < max(1_int64, rate)) return
    end if
    progress_thread_generation = generation
    progress_thread_count = count
    progress_thread_rate = rate
    call decoder_ipc_progress_report(generation)
  end subroutine write_decode_progress

  subroutine enable_decode_evidence_records()
    evidence_records_enabled = .true.
  end subroutine enable_decode_evidence_records

  ! Writes, ahead of an FT8 or FT4 decode line,
  !   <DecodeEvidence> start=S freq=F bits=B
  ! S: seconds from the start of the input (the period) to the start of the
  !    message's first symbol, or 'none'.
  ! F: audio frequency of tone 0, Hz.
  ! B: 20 hex digits of the 77 received message bits, first bit most
  !    significant, then three zero bits; 'none' when the bits were not
  !    received as such (a7/a8 hypotheses).
  subroutine write_decode_evidence_record(has_start, start_seconds, frequency_hz, &
       has_payload, payload77)
    use iso_fortran_env, only: int8
    logical, intent(in) :: has_start, has_payload
    real, intent(in) :: start_seconds, frequency_hz
    integer(int8), intent(in) :: payload77(77)
    character(len=16), parameter :: digits = '0123456789ABCDEF'
    character(len=20) :: bits
    character(len=24) :: start, freq
    integer :: i, j, nibble

    if (.not. evidence_records_enabled) return
    bits = 'none'
    if (has_payload) then
       do i = 1, 20
          nibble = 0
          do j = 1, 4
             nibble = 2 * nibble
             if (4 * (i - 1) + j <= 77) then
                if (payload77(4 * (i - 1) + j) /= 0) nibble = nibble + 1
             end if
          end do
          bits(i:i) = digits(nibble + 1:nibble + 1)
       end do
    end if
    start = 'none'
    if (has_start) write(start, '(f0.5)') start_seconds
    write(freq, '(f0.5)') frequency_hz
    write(*,'(6a)') '<DecodeEvidence> start=', trim(adjustl(start)), &
         ' freq=', trim(adjustl(freq)), ' bits=', trim(bits)
  end subroutine write_decode_evidence_record

end module decode_completion_module

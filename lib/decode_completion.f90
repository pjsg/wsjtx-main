module decode_completion_module
  use iso_fortran_env, only: int8, int64
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
  public :: record_decoded_bits,record_decoded_words,clear_decoded_bits
  public :: take_decoded_bits,write_decoded_bits_record,take_decoded_bytes_c

  ! Under the GUI, jt9 precedes an FT8 or FT4 decode line with what the
  ! decoder measured: message start, precise frequency, and message bits.
  ! Standalone jt9 output is left unchanged.
  logical, save :: evidence_records_enabled = .false.

  ! Message bits a decoder received, held from just before it reports the
  ! decode until the report takes them. Decoders record only bits received as
  ! such (not deep-search, list or averaged results) and clear the slot
  ! otherwise. One slot per thread: a decoder and its reports share a thread.
  integer, parameter :: MAX_DECODED_BITS = 128
  integer(int8), save :: decoded_bits(MAX_DECODED_BITS) = 0_int8
  integer, save :: decoded_bit_count = 0
!$omp threadprivate(decoded_bits, decoded_bit_count)

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

  ! Bits as 0/1 values, first bit first.
  subroutine record_decoded_bits(bits)
    integer(int8), intent(in) :: bits(:)

    decoded_bit_count = min(size(bits), MAX_DECODED_BITS)
    decoded_bits(1:decoded_bit_count) = bits(1:decoded_bit_count)
  end subroutine record_decoded_bits

  ! nwords words of width bits each, most significant bit first: the 72-bit
  ! JT4, JT9 and JT65 messages are twelve 6-bit words.
  subroutine record_decoded_words(words, nwords, width)
    integer, intent(in) :: nwords, width
    integer, intent(in) :: words(nwords)
    integer :: i, j

    decoded_bit_count = min(nwords * width, MAX_DECODED_BITS)
    do i = 1, nwords
       do j = 1, width
          if ((i - 1) * width + j > decoded_bit_count) exit
          decoded_bits((i - 1) * width + j) = &
               int(ibits(words(i), width - j, 1), int8)
       end do
    end do
  end subroutine record_decoded_words

  subroutine clear_decoded_bits()
    decoded_bit_count = 0
  end subroutine clear_decoded_bits

  ! Returns the recorded bits, if any, and clears the slot.
  subroutine take_decoded_bits(bits, nbits)
    integer(int8), intent(out) :: bits(MAX_DECODED_BITS)
    integer, intent(out) :: nbits

    nbits = decoded_bit_count
    bits = 0_int8
    bits(1:nbits) = decoded_bits(1:nbits)
    decoded_bit_count = 0
  end subroutine take_decoded_bits

  ! Writes '<DecodeEvidence> start=none freq=none bits=B' ahead of a decode
  ! line, B being the bits as hex, first bit most significant, padded with
  ! zero bits to whole bytes. Nothing is written for no bits.
  subroutine write_decoded_bits_record(bits, nbits)
    integer(int8), intent(in) :: bits(:)
    integer, intent(in) :: nbits

    if (.not. evidence_records_enabled .or. nbits <= 0) return
    write(*,'(2a)') '<DecodeEvidence> start=none freq=none bits=', hex_bits(bits, nbits)
  end subroutine write_decoded_bits_record

  function hex_bits(bits, nbits) result(hex)
    integer(int8), intent(in) :: bits(:)
    integer, intent(in) :: nbits
    character(len=:), allocatable :: hex
    character(len=16), parameter :: digits = '0123456789ABCDEF'
    integer :: i, j, k, nibble, ndigits

    ndigits = 2 * ((nbits + 7) / 8)
    allocate(character(len=ndigits) :: hex)
    do i = 1, ndigits
       nibble = 0
       do j = 1, 4
          k = 4 * (i - 1) + j
          nibble = 2 * nibble
          if (k <= nbits) then
             if (bits(k) /= 0) nibble = nibble + 1
          end if
       end do
       hex(i:i) = digits(nibble + 1:nibble + 1)
    end do
  end function hex_bits

  ! For in-process decoders (MSK144): the recorded bits packed most
  ! significant bit first into whole bytes, at most 16; clears the slot.
  subroutine take_decoded_bytes_c(bytes, nbytes) bind(C, name='take_decoded_bytes_c')
    use iso_c_binding, only: c_signed_char, c_int
    integer(c_signed_char), intent(out) :: bytes(16)
    integer(c_int), intent(out) :: nbytes
    integer(int8) :: bits(MAX_DECODED_BITS)
    integer :: nbits, i, j, v

    call take_decoded_bits(bits, nbits)
    nbits = min(nbits, 128)
    nbytes = (nbits + 7) / 8
    bytes = 0
    do i = 1, nbytes
       v = 0
       do j = 1, 8
          v = 2 * v
          if (8 * (i - 1) + j <= nbits) then
             if (bits(8 * (i - 1) + j) /= 0) v = v + 1
          end if
       end do
       if (v > 127) v = v - 256
       bytes(i) = int(v, c_signed_char)
    end do
  end subroutine take_decoded_bytes_c

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

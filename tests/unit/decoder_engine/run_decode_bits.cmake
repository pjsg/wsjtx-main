# Decodes SAMPLES with jt9 --evidence and checks every reported bit string
# unpacks to the message decoded with it.
foreach (required JT9 CHECKER BITS WORK_DIR SOURCE_DIR ARGS SAMPLES)
  if (NOT DEFINED ${required})
    message (FATAL_ERROR "${required} is required")
  endif ()
endforeach ()

string (REPLACE "|" ";" args "${ARGS}")
string (REPLACE "|" ";" samples "${SAMPLES}")
file (REMOVE_RECURSE "${WORK_DIR}")
file (MAKE_DIRECTORY "${WORK_DIR}")
execute_process (
  COMMAND "${JT9}" --evidence -a "${WORK_DIR}" -t "${WORK_DIR}" -r "${SOURCE_DIR}" ${args} ${samples}
  WORKING_DIRECTORY "${WORK_DIR}"
  OUTPUT_FILE "${WORK_DIR}/decodes.txt"
  RESULT_VARIABLE decoder_result)
if (NOT decoder_result EQUAL 0)
  message (FATAL_ERROR "jt9 exited with ${decoder_result}")
endif ()
execute_process (
  COMMAND "${CHECKER}" ${BITS} "${WORK_DIR}/decodes.txt"
  RESULT_VARIABLE check_result
  OUTPUT_VARIABLE check_output
  ERROR_VARIABLE check_output)
message ("${check_output}")
if (NOT check_result EQUAL 0)
  file (READ "${WORK_DIR}/decodes.txt" decodes)
  message (FATAL_ERROR "message bits check failed\n${decodes}")
endif ()

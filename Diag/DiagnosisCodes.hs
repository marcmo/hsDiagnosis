{- | common diagnosis return values. -}
module Diag.DiagnosisCodes where 

import Data.Word

positive_response_offset 	            	  = 0x40 :: Word8
negative_response_identifier              = 0x7f :: Word8
positive_response                         = 0x00 :: Word8
positive_response_wait_for_eeprom_finish  = 0x00 :: Word8
min_diag_message_length                   = 3 :: Word8
negative_response_message_length          = 3 :: Word8
negative_response_service_offset          = 1 :: Word8
negative_response_errorcode_offset        = 2 :: Word8

data DiagRequestCode =
	REQUEST_DIAGNOSTIC_SESSION_CONTROL
	| REQUEST_ECU_RESET
	| REQUEST_READ_DTC_INFORMATION
	| REQUEST_READ_DATA_BY_IDENTIFIER
	| REQUEST_WRITE_DATA_BY_IDENTIFIER
	| REQUEST_SECURITY_ACCESS
	| REQUEST_COMMUNICATION_CONTROL
	| REQUEST_ROUTINE_CONTROL
	| REQUEST_REQUEST_DOWNLOAD
	| REQUEST_REQUEST_UPLOAD
	| REQUEST_TRANSFER_DATA
	| REQUEST_TRANSFER_EXIT
	| REQUEST_TESTER_PRESENT
	| REQUEST_CONTROL_DTC_SETTING
	| REQUEST_RESPONSE_ON_EVENT
	| REQUEST_CLEAR_DIAGNOSTIC_INFORMATION
 
requestToCode = [ 
                (REQUEST_DIAGNOSTIC_SESSION_CONTROL,0x10),
                (REQUEST_ECU_RESET,0x11),
                (REQUEST_READ_DTC_INFORMATION,0x19),
                (REQUEST_READ_DATA_BY_IDENTIFIER,0x22),
                (REQUEST_WRITE_DATA_BY_IDENTIFIER,0x2E),
                (REQUEST_SECURITY_ACCESS,0x27),
                (REQUEST_COMMUNICATION_CONTROL,0x28),
                (REQUEST_ROUTINE_CONTROL,0x31),
                (REQUEST_REQUEST_DOWNLOAD,0x34),
                (REQUEST_REQUEST_UPLOAD,0x35),
                (REQUEST_TRANSFER_DATA,0x36),
                (REQUEST_TRANSFER_EXIT,0x37),
                (REQUEST_TESTER_PRESENT,0x3E),
                (REQUEST_CONTROL_DTC_SETTING,0x85),
                (REQUEST_RESPONSE_ON_EVENT,0x86),
                (REQUEST_CLEAR_DIAGNOSTIC_INFORMATION,0x14)
              ]
codeToRequest = map (\(x, y) -> (y, x)) requestToCode

id_routine_control_start_routine      = 0x01 :: Word8
id_routine_control_stop_routine       = 0x02 :: Word8
id_routine_control_get_routine_result = 0x03 :: Word8
 
service_id_length     = 1 :: Word8
subfunction_id_length = 1 :: Word8
routine_id_length     = 2 :: Word8
data_id_length        = 2 :: Word8

{- | Return codes used throughout diagnosis framework.
 - @note
 - The prefix ISO_ indicates that this is an official ISO14229 code.
 - 
 - @imporant
 - The DiagReturnCode::Type NOT_RESPONSIBLE has a special meaning. Returning
 - any other value from verify indicates that the job has taken control
 - over the request and will handle it. No further job will be notified
 - of the request in question!
 -}

data DiagErrorCode = 
              ISO_GENERAL_REJECT
              | ISO_SERVICE_NOT_SUPPORTED
              | ISO_SUBFUNCTION_NOT_SUPPORTED
              | ISO_INVALID_FORMAT
              | ISO_RESPONSE_TOO_LARGE
              | ISO_BUSY_REPEAT_REQUEST
              | ISO_CONDITIONS_NOT_CORRECT
              | ISO_REQUEST_SEQUENCE_ERROR
              | ISO_CONTROL_UNIT_ON_SUBBUS_NOT_RESPONDING
              | ISO_REQUEST_OUT_OF_RANGE
              | ISO_SECURITY_ACCESS_DENIED
              | ISO_INVALID_KEY
              | ISO_EXCEEDED_NUMS_OF_ATTEMPTS
              | ISO_UPLOAD_DOWNLOAD_NOT_ACCEPTED
              | ISO_GENERAL_PROGRAMMING_FAILURE
              | ISO_WRONG_BLOCK_SEQUENCE_COUNTER
              | ISO_RESPONSE_PENDING
              | ISO_SUBFUNCTION_NOT_SUPPORTED_IN_ACTIVE_SESSION
              | ISO_SERVICE_NOT_SUPPORTED_IN_ACTIVE_SESSION
              | NOT_RESPONSIBLE         -- ^ The job verifying a request is not responsible for handling it
              | DIAG_OK                 -- ^ The job wants to handle the request
                deriving (Eq, Show, Read)

codeOfError :: DiagErrorCode -> Word8
codeOfError f = case lookup f errorToCodeAndName of
                Just x -> fst x
                _ -> error $ "Internal error in codeOfError"
nameOfError :: Word8 -> String
nameOfError f = case lookup f codeToErrorName of
                Just x -> x
                _ -> error $ "Internal error in codeOfError"

errorOfCode :: Word8 -> DiagErrorCode
errorOfCode f = case lookup f codeToError of
                Just x -> x
                _ -> error $ "Invalid code in errorOfCode"
errorToCodeAndName = [ 
                      (ISO_GENERAL_REJECT,(0x10,"ISO_GENERAL_REJECT")),
                      (ISO_SERVICE_NOT_SUPPORTED,(0x11,"ISO_SERVICE_NOT_SUPPORTED")),
                      (ISO_SUBFUNCTION_NOT_SUPPORTED,(0x12,"ISO_SUBFUNCTION_NOT_SUPPORTED")),
                      (ISO_INVALID_FORMAT,(0x13,"ISO_INVALID_FORMAT")),
                      (ISO_RESPONSE_TOO_LARGE,(0x14,"ISO_RESPONSE_TOO_LARGE")),
                      (ISO_BUSY_REPEAT_REQUEST,(0x21,"ISO_BUSY_REPEAT_REQUEST")),
                      (ISO_CONDITIONS_NOT_CORRECT,(0x22,"ISO_CONDITIONS_NOT_CORRECT")),
                      (ISO_REQUEST_SEQUENCE_ERROR,(0x24,"ISO_REQUEST_SEQUENCE_ERROR")),
                      (ISO_CONTROL_UNIT_ON_SUBBUS_NOT_RESPONDING,(0x25,"ISO_CONTROL_UNIT_ON_SUBBUS_NOT_RESPONDING")),
                      (ISO_REQUEST_OUT_OF_RANGE,(0x31,"ISO_REQUEST_OUT_OF_RANGE")),
                      (ISO_SECURITY_ACCESS_DENIED,(0x33,"ISO_SECURITY_ACCESS_DENIED")),
                      (ISO_INVALID_KEY,(0x35,"ISO_INVALID_KEY")),
                      (ISO_EXCEEDED_NUMS_OF_ATTEMPTS,(0x36,"ISO_EXCEEDED_NUMS_OF_ATTEMPTS")),
                      (ISO_UPLOAD_DOWNLOAD_NOT_ACCEPTED,(0x70,"ISO_UPLOAD_DOWNLOAD_NOT_ACCEPTED")),
                      (ISO_GENERAL_PROGRAMMING_FAILURE,(0x72,"ISO_GENERAL_PROGRAMMING_FAILURE")),
                      (ISO_WRONG_BLOCK_SEQUENCE_COUNTER,(0x73,"ISO_WRONG_BLOCK_SEQUENCE_COUNTER")),
                      (ISO_RESPONSE_PENDING,(0x78,"ISO_RESPONSE_PENDING")),
                      (ISO_SUBFUNCTION_NOT_SUPPORTED_IN_ACTIVE_SESSION,(0x7E,"ISO_SUBFUNCTION_NOT_SUPPORTED_IN_ACTIVE_SESSION")),
                      (ISO_SERVICE_NOT_SUPPORTED_IN_ACTIVE_SESSION,(0x7F,"ISO_SERVICE_NOT_SUPPORTED_IN_ACTIVE_SESSION")),
                      (NOT_RESPONSIBLE,(0xFE,"NOT_RESPONSIBLE")),
                      (DIAG_OK,(0xFF,"DIAG_OK"))
           ]

codeToError = map (\(x, (y,name)) -> (y, x)) errorToCodeAndName
codeToErrorName = map (\(x, (y,name)) -> (y, name)) errorToCodeAndName


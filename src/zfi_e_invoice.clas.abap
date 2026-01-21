CLASS zfi_e_invoice DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .
    CLASS-DATA : final_message_irn TYPE string .
    CLASS-DATA : final_message_ewaybill TYPE string .

    CLASS-METHODS :
      get_table_fields
        IMPORTING VALUE(invoice)     TYPE CHAR10
                  VALUE(companycode) TYPE CHAR4
                  VALUE(year)        TYPE string
                  VALUE(irngenrate)  TYPE string
        RETURNING VALUE(result)     TYPE string.
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZFI_E_INVOICE IMPLEMENTATION.


  METHOD get_table_fields .
    DATA(lv_string)  =  zfi_invoice_irn_data=>get_table_fields( invoice = invoice year = year companycode = companycode  ) . .
    DATA(result9)  = zsd_authentication_token_irn=>generate_authentication_token( billingdocument = invoice companycode = CompanyCode lv_string = lv_string
                                                                             irngenrate = irngenrate year = year )  .


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
REPLACE ALL OCCURRENCES OF '[{"custom_fields"' IN result9  WITH '{"custom_fields"'.
      REPLACE ALL OCCURRENCES OF 'nll}]' IN result9  WITH 'nll}'.

DATA :  wa_j_1ig_invrefnum TYPE Y1ig_invrefnum .

FIELD-SYMBOLS:
       <data>               TYPE data,
       <data1>              TYPE data,
       <data2>              TYPE data,
       <data3>              TYPE data,
       <data4>              TYPE any ,
       <field>              TYPE any ,
       <field_ackdt>        TYPE any,
       <field_ackno>        TYPE any ,
       <field_irn>          TYPE any ,
       <field_signedinv>    TYPE any ,
       <field_signedqr>     TYPE any ,
       <field_status>       TYPE any ,
       <field_success>      TYPE any ,
       <field_info>         TYPE any .

DATA IRN TYPE STRING .
DATA ACKDT TYPE STRING .
DATA ACKNO TYPE STRING .
DATA SIGNDINV TYPE STRING .
DATA SIGNDQR TYPE STRING .
DATA STATUS1 TYPE STRING .
DATA SUCCESS TYPE STRING .

DATA(lr_d1) = /ui2/cl_json=>generate( json = result9 ).
   IF lr_d1 IS BOUND.
 ASSIGN lr_d1->* TO <data>.

    ASSIGN COMPONENT `govt_response` OF STRUCTURE <data>  TO   <field>    .
    if sy-subrc = 0 .
    ASSIGN <field>->* TO <data2>  .
    if sy-subrc = 0 .
*     LVT  =  <data2> .
    ASSIGN COMPONENT `Irn` OF STRUCTURE <data2>  TO   <field_irn>    .
   if sy-subrc = 0 .
    ASSIGN <field_irn>->* TO <field_irn> .
    IRN = <field_irn> .

    ASSIGN COMPONENT `AckDt` OF STRUCTURE <data2>  TO   <field_ackdt>    .
    ASSIGN  <field_ackdt>->* TO  <field_ackdt> .
    ACKDT =  <field_ackdt> .

    ASSIGN COMPONENT `AckNo` OF STRUCTURE <data2>  TO   <field_ackno>     .
    ASSIGN  <field_ackno>->* TO  <field_ackno>  .
    ACKNO =  <field_ackno>  .

    ASSIGN COMPONENT `SignedInvoice` OF STRUCTURE <data2>  TO   <field_signedinv>     .
    ASSIGN  <field_signedinv>->* TO  <field_signedinv>  .
    SIGNDINV =  <field_signedinv>  .

    ASSIGN COMPONENT `SignedQRCode` OF STRUCTURE <data2>  TO   <field_signedqr>     .
    ASSIGN  <field_signedqr>->* TO  <field_signedqr>  .
    SIGNDQR =  <field_signedqr>  .

    ASSIGN COMPONENT `Status` OF STRUCTURE <data2>  TO   <field_status>     .
    ASSIGN  <field_status>->* TO  <field_status>  .
    STATUS1 =  <field_status>  .

    ASSIGN COMPONENT `Success` OF STRUCTURE <data2>  TO   <field_success>     .
    ASSIGN  <field_success>->* TO  <field_success>  .
    SUCCESS =  <field_success>  .
ENDIF.
   endif .

SELECT SINGLE AccountingDocumentType  FROM i_operationalacctgdocitem  WHERE accountingdocument = @invoice
    AND CompanyCode = @companycode AND FiscalYear = @year INTO   @DATA(AccountingDocumentType).
    IF AccountingDocumentType = 'DR' .
    DATA(doctyp) = 'INV' .
    ELSEIF AccountingDocumentType = 'DG' .
    doctyp = 'CRN' .
    ELSEIF AccountingDocumentType = 'DC'  OR AccountingDocumentType = 'DN'.
    doctyp = 'DRN' .
    ENDIF.


    IF IRN IS NOT INITIAL.

      wa_j_1ig_invrefnum-bukrs = companycode .
      invoice         =    |{ invoice ALPHA = in }|.
      wa_j_1ig_invrefnum-docno = invoice .
      wa_j_1ig_invrefnum-doc_year = year.
      wa_j_1ig_invrefnum-doc_type = doctyp.
      wa_j_1ig_invrefnum-irn = IRN.
      wa_j_1ig_invrefnum-odn_date = sy-datum.
      wa_j_1ig_invrefnum-ack_no = ACKNO.
      wa_j_1ig_invrefnum-ack_date = ACKDT.
      wa_j_1ig_invrefnum-irn_status = STATUS1 .
      wa_j_1ig_invrefnum-signed_inv    =  SIGNDINV .
      wa_j_1ig_invrefnum-signed_qrcode =  SIGNDQR .
   "   wa_j_1ig_invrefnum-vehno         =  docs1-vehno.
   "   wa_j_1ig_invrefnum-distance      =  distan.
      MODIFY Y1ig_invrefnum FROM @wa_j_1ig_invrefnum  .
      COMMIT WORK AND WAIT.

 final_message_irn = |Part 1 Irn Suscessfully Generate IRN { IRN } ' ' Ackno { ACKNO }  Ackdate {  ACKDT }   | .

    Else .


REPLACE ALL OCCURRENCES OF '{"custom_fields":null,"deleted":false,"document_status":"NOT_CREATED","error_response":null,"errors":null,"govt_response":{"Success":"N","ErrorDetails":' IN result9  WITH '['.
      REPLACE ALL OCCURRENCES OF 'nll}]' IN result9  WITH 'nll}'.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
final_message_irn = result9 .

 endif .
    result = final_message_irn .

else .
  result = result9 .

ENDIF.
ENDIF.
 ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
  data(result) = get_table_fields( invoice = '1800000052' irngenrate = ' '  year = '2023' companycode = '1000')  .
  ENDMETHOD.
ENDCLASS.

CLASS yeinvoice_te DEFINITION
  PUBLIC

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .

    CLASS-DATA : access_token TYPE string .
    CLASS-DATA : final_message_irn TYPE string .

    CLASS-METHODS :

      "CLASS FOR IRN GENERATION
      get_table_fields
        IMPORTING VALUE(invoice)       TYPE CHAR10
                  VALUE(companycode)   TYPE CHAR4
                  VALUE(irngenrate)    TYPE string
                  VALUE(eway_generate) TYPE string
                  VALUE(distance)      TYPE string OPTIONAL
                  transpoter_name      TYPE string OPTIONAL
                  transportid          TYPE string OPTIONAL
                  transportdoc         TYPE string OPTIONAL
                  vehiclenumber        TYPE string OPTIONAL
        RETURNING VALUE(result)        TYPE string ,
        create_client
        IMPORTING url           TYPE string
        RETURNING VALUE(result) TYPE REF TO if_web_http_client
        RAISING   cx_static_check .

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS YEINVOICE_TE IMPLEMENTATION.


   METHOD create_client.       "    "
    DATA(dest) = cl_http_destination_provider=>create_by_url( url ).
    result = cl_web_http_client_manager=>create_by_http_destination( dest ).
  ENDMETHOD.


  METHOD get_table_fields .

  IF irngenrate = 'X' .
*****************************************************************IRN DATA***************************************8
 SELECT single *  FROM i_billingdocumentbasic  where BillingDocument = @invoice INTO   @DATA(bIll_head).


   data(lv_string)   =  zsd_invoice_irn_data=>get_table_fields( companycode = companycode invoice = invoice irngenrate = irngenrate eway_generate = eway_generate
                                      distance = distance transpoter_name = transpoter_name transportid = transportid transportdoc = transportdoc
                                      vehiclenumber = vehiclenumber ) .


    CLEAR   access_token .
    DATA(result9)  = zsd_authentication_token_irn=>generate_authentication_token( billingdocument = invoice companycode = CompanyCode lv_string = lv_string
                                                                                  irngenrate = irngenrate )  .

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    REPLACE ALL OCCURRENCES OF '[{"custom_fields"' IN result9 WITH '{"custom_fields"'.
    REPLACE ALL OCCURRENCES OF 'nll}]' IN result9 WITH 'nll}'.

DATA :  wa_j_1ig_invrefnum TYPE Y1ig_invrefnum .

FIELD-SYMBOLS:
       <data>               TYPE data,
       <data1>              TYPE data,
       <dataG1>             TYPE data,
       <dataG2>             TYPE data,
       <data2>              TYPE data,
       <data3>              TYPE data,
       <data4>              TYPE any ,
       <field>              TYPE any ,
       <field_dup>          TYPE any,
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

    data km tyPE string.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""for distance
TYPES: BEGIN OF ty_info,
         infcd TYPE string,
         desc  TYPE string,
       END OF ty_info.

TYPES: tt_info TYPE STANDARD TABLE OF ty_info WITH EMPTY KEY.

TYPES: BEGIN OF ty_govt_response,
         info          TYPE tt_info,
       END OF ty_govt_response.

TYPES: BEGIN OF ty_payload,
         govt_response TYPE ty_govt_response,
       END OF ty_payload.
DATA(ls_payload) = VALUE ty_payload( ).
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

   DATA(lr_d1) = /ui2/cl_json=>generate( json = result9 ).
   IF lr_d1 IS BOUND.
   ASSIGN lr_d1->* TO <data>.

    ASSIGN COMPONENT `govt_response` OF STRUCTURE <data>  TO   <field>    .
    if sy-subrc = 0 .
    ASSIGN <field>->* TO <data2>  .
    ENDIF.

    ASSIGN COMPONENT `Irn` OF STRUCTURE <data2>  TO   <field_irn>    .
   if sy-subrc = 0 .
    ASSIGN <field_irn>->* TO <field_irn> .
    IRN = <field_irn> .
    ENDIF.
    ASSIGN COMPONENT `AckDt` OF STRUCTURE <data2>  TO   <field_ackdt>    .
    if sy-subrc = 0 .
    ASSIGN  <field_ackdt>->* TO  <field_ackdt> .
    ACKDT =  <field_ackdt> .
    ENDIF.
    ASSIGN COMPONENT `AckNo` OF STRUCTURE <data2>  TO   <field_ackno>     .
    if sy-subrc = 0 .
    ASSIGN  <field_ackno>->* TO  <field_ackno>  .
    ACKNO =  <field_ackno>  .
    ENDIF.
    ASSIGN COMPONENT `SignedInvoice` OF STRUCTURE <data2>  TO   <field_signedinv>     .
    if sy-subrc = 0 .
    ASSIGN  <field_signedinv>->* TO  <field_signedinv>  .
    SIGNDINV =  <field_signedinv>  .
    ENDIF.
    ASSIGN COMPONENT `SignedQRCode` OF STRUCTURE <data2>  TO   <field_signedqr>     .
    if sy-subrc = 0 .
    ASSIGN  <field_signedqr>->* TO  <field_signedqr>  .
    SIGNDQR =  <field_signedqr>  .
    ENDIF.
    ASSIGN COMPONENT `Status` OF STRUCTURE <data2>  TO   <field_status>     .
    if sy-subrc = 0 .
    ASSIGN  <field_status>->* TO  <field_status>  .
    STATUS1 =  <field_status>  .
   ENDIF.
    ASSIGN COMPONENT `Success` OF STRUCTURE <data2>  TO   <field_success>     .
    if sy-subrc = 0 .
    ASSIGN  <field_success>->* TO  <field_success>  .
    SUCCESS =  <field_success>  .
    ENDIF.
    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""for distance""""""""""""""""""""
        /ui2/cl_json=>deserialize(
         EXPORTING
           json = result9
         CHANGING
           data = ls_payload
       ).
       DATA lv_distance TYPE N LENGTH 10 .
        LOOP AT ls_payload-govt_response-info  INTO DATA(ls_value).

         FIND REGEX '(\d+)' IN ls_value-desc SUBMATCHES lv_distance.
         km = lv_distance.
*        EWAYBILLDATA-distance = lv_distance.
*        MODIFY yj1ig_ewaybill FROM @EWAYBILLDATA  .
        ENDLOOP.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    IF IRN IS NOT INITIAL.

 select single billingdocumenttype, companycode ,FiscalYear
  from I_BillingDocumentBasic where BillingDocument = @invoice AND CompanyCode = @companycode into @data(comp1)   .
      wa_j_1ig_invrefnum-bukrs = comp1-CompanyCode .
      invoice                           =    |{ invoice ALPHA = in }|.
      wa_j_1ig_invrefnum-docno         = invoice .
      wa_j_1ig_invrefnum-doc_year      = comp1-FiscalYear..
      wa_j_1ig_invrefnum-doc_type      = comp1-BillingDocumentType.
      wa_j_1ig_invrefnum-irn           = IRN.
      wa_j_1ig_invrefnum-odn_date      = sy-datum.
      wa_j_1ig_invrefnum-ack_no        = ACKNO.
      wa_j_1ig_invrefnum-ack_date      = ACKDT.
      wa_j_1ig_invrefnum-irn_status    = STATUS1 .
      wa_j_1ig_invrefnum-signed_inv    =  SIGNDINV .
      wa_j_1ig_invrefnum-signed_qrcode =  SIGNDQR .
      wa_j_1ig_invrefnum-vehno         =  invoice.
      wa_j_1ig_invrefnum-distance      =  km . "distance.
      wa_j_1ig_invrefnum-lr_no         =  transportdoc.
  IF invoice IS INITIAL.
   wa_j_1ig_invrefnum-vehno = vehiclenumber.
   ENDIF.
      MODIFY Y1ig_invrefnum FROM @wa_j_1ig_invrefnum  .
      COMMIT WORK AND WAIT.

 final_message_irn = |Part 1 Irn Suscessfully Generated IRN { IRN } ' ' Ackno { ACKNO }  Ackdate {  ACKDT }   | .
 Else .
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
 REPLACE ALL OCCURRENCES OF '{"custom_fields":null,"deleted":false,"document_status":"NOT_CREATED","error_response":null,"errors":null,"govt_response":{"Success":"N","ErrorDetails":' IN result9  WITH '['.
      REPLACE ALL OCCURRENCES OF 'nll}]' IN result9  WITH 'nll}'.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
 final_message_irn = result9 .
 endif .
 Endif.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*****************************************************************IRN DATA END***************************************8
    SELECT SINGLE * FROM y1ig_invrefnum WHERE  docno = @invoice AND bukrs = @companycode INTO  @DATA(details) .
*****************************************************************EWAY-BILL DATA***************************************8
    IF details-irn IS NOT INITIAL AND details-irn_status = 'ACT' AND eway_generate = 'X' .

    DATA(status) = zsd_generate_ewaybill=>generate_ewaybill(
                                      billingdocument = invoice       distance = distance
                                      transportdoc    = transportdoc  transportid = transportid
                                      vehiclenumber   = vehiclenumber companycode     =  companycode
                                      transpoter_name = transpoter_name eway_generate = eway_generate )  .

 ELSEIF details-irn IS INITIAL AND details-irn_status <> 'ACT' AND eway_generate = 'X' .

   status = yeway_bill_without_irn=>generated_eway_bill( invoice   = invoice companycode = companycode
                                  distance        = DISTANCE       Eway_generate = Eway_generate
                                  transportdoc    = transportdoc
                                  transportid     = transportid
                                  vehiclenumber   = Vehiclenumber
                                  transpoter_name = transpoter_name )  .
ENDIF .
*****************************************************************EWAY-BILL DATA***************************************8
    CONCATENATE '***Irn****' final_message_irn INTO DATA(msz1)  .

    CONCATENATE '***Eway****PART_TWO' status INTO DATA(msz2) .

 if  eway_generate = 'X' AND irngenrate = 'X' .
 CONCATENATE msz1  '///////////////////'  msz2 INTO DATA(errormsz) .

 ELSEif eway_generate = 'X' AND irngenrate <> 'X' .

 IF vehiclenumber IS NOT INITIAL .

 CONCATENATE '***Eway****PART B' status INTO msz2 .

 ELSE.

 CONCATENATE '***Eway****PART A' status INTO msz2 .

 ENDIF.
 errormsz  = msz2.

 ELSEIF irngenrate = 'X' AND eway_generate <> 'X'.
 CONCATENATE '***Irn****' final_message_irn INTO msz1  .
 errormsz  = msz1.
 ENDIF.

 result  =  errormsz .
 ENDMETHOD .


  METHOD if_oo_adt_classrun~main.
  ENDMETHOD.
ENDCLASS.

CLASS zeway_print DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

 PUBLIC SECTION.
   INTERFACES if_oo_adt_classrun .

   CLASS-DATA : access_token TYPE string .

 class-methods :
                get_error
                IMPORTING
                VALUE(variable)  type char10
                RETURNING VALUE(retu) type string  .

    CLASS-METHODS :
      read_posts
        IMPORTING
                  VALUE(variable)  type char10
        RETURNING VALUE(result12) TYPE string
        RAISING   cx_static_check ,

      create_client
        IMPORTING url           TYPE string
        RETURNING VALUE(result) TYPE REF TO if_web_http_client
        RAISING   cx_static_check.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZEWAY_PRINT IMPLEMENTATION.


    METHOD create_client.
    DATA(dest) = cl_http_destination_provider=>create_by_url( url ).
    result = cl_web_http_client_manager=>create_by_http_destination( dest ).
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

  DATA(pdf1) = me->read_posts( variable = 'RJ25100068' ) .

  ENDMETHOD.


  METHOD GET_ERROR.

DATA: lv_pdf_text   TYPE string,
      lv_pdf_base64 TYPE string,
      lv_invoice_line TYPE string.

lv_invoice_line = '(Invoice No: ' && variable && ') Tj'.

CONCATENATE
  '%PDF-1.4'
  '1 0 obj'
  '<< /Type /Catalog /Pages 2 0 R >>'
  'endobj'
  '2 0 obj'
  '<< /Type /Pages /Kids [3 0 R] /Count 1 >>'
  'endobj'
  '3 0 obj'
  '<< /Type /Page /Parent 2 0 R /MediaBox [0 0 595 842]'
  '   /Contents 4 0 R'
  '   /Resources << /Font << /F1 5 0 R >> >> >>'
  'endobj'
  '4 0 obj'
  '<< /Length 2000 >>'
  'stream'
  'BT'
  '/F1 24 Tf'
  '30 TL'
  '200 500 Td'
  '(E-way Not Generated For) Tj'
  'T*'
  lv_invoice_line
  'ET'
  'endstream'
  'endobj'
  '5 0 obj'
  '<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>'
  'endobj'
  'xref'
  '0 6'
  '0000000000 65535 f'
  'trailer'
  '<< /Root 1 0 R >>'
  '%%EOF'
INTO lv_pdf_text
SEPARATED BY cl_abap_char_utilities=>newline.

DATA : lv_pdf_xstring TYPE xstring.
lv_pdf_base64 =  cl_web_http_utility=>encode_base64( lv_pdf_text ).
retu          = lv_pdf_base64.

  ENDMETHOD.


  METHOD read_posts.

 DATA: vbeln1 TYPE c LENGTH 10.
       vbeln1  =   |{ variable ALPHA = IN }|.
* DATA(invoice1) = | { invoice alpha = in }|.

 DATA GSTIN TYPE STRING.
 data token TYPE string.

  DATA(BRCT_OPEN)  = '{'.
  DATA(BRCT_CLOSE) = '}'.

 "SELECT SINGLE * FROM yj1ig_ewaybill WHERE docno = @vbeln1 INTO @DATA(EWAY).

  SELECT SINGLE * FROM zsd_plant_address AS a INNER JOIN i_billingdocumentitembasic AS b ON ( a~plant =  b~plant )
    WHERE billingdocument =  @vbeln1 INTO @DATA(plantaddress) .
 SELECT SINGLE * FROM YJ1IG_EWAYBILLDD WHERE docno = @vbeln1 AND Status <> 'C' INTO @DATA(EWAY).

if eway is not inITIAL .

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

   DATA(EJSON) = | { BRCT_OPEN } "ewb_numbers":[ { EWAY-ebillno } ], "print_type": "DETAILED" { BRCT_CLOSE } |.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
     IF sy-sysiD = 'ALU'  OR SY-SYSID = 'AWL'.
      DATA : ewaylink  TYPE  string VALUE 'https://api-sandbox.clear.in/einv/v2/eInvoice/ewaybill/print?format=PDF'  .
      DATA : uuid TYPE string.

      uuid = cl_system_uuid=>create_uuid_x16_static(  ).

      DATA(url) = |{ ewaylink }|.
      DATA(client) = create_client( url ).
      DATA(req) = client->get_http_request(  ).

      token = 'sandbox.3.1a4cace2-95aa-404a-8cba-ccdffb3cab60_64d9d8792317a543e97606345ca64f8e951e60e7042d547e5886267a98ed13bc'.
      GSTIN = '08AAFCD5862R018'.

      uuid = cl_system_uuid=>create_uuid_x16_static(  ).
      url = |{ ewaylink }|.
      client = create_client( url ).

      req = client->get_http_request(  ).
      req->set_header_field( i_name = 'gstin'    i_value = GSTIN  ).
      req->set_header_field( i_name = 'X-Cleartax-Auth-Token'    i_value = token ).
      req->set_content_type( 'application/json' ).

      req->set_text( ejson ) .
      DATA: result9 TYPE xstring.
      result9 = client->execute( if_web_http_client=>post )->to_xstring(  ) .
      DATA(ls_data_xml2) = cl_web_http_utility=>encode_x_base64( result9 ).
      RESULT12 = ls_data_xml2.
      client->close(  )  .

    ELSE.

   ewaylink = 'https://api.clear.in/einv/v2/eInvoice/ewaybill/print?format=PDF' .

    SELECT SINGLE * FROM zsales_tmg WHERE plant = @plantaddress-a-Plant INTO @data(irngst).
    GSTIN = irngst-gstno.
    token = '3.aa0ee9a8-d5ad-4e0c-a7b2-e721f50eab8e_7967787bfb61cc0c0c8670668322ad9bad7b875dbd0ce5717e0cb911389c89ed'.

      uuid = cl_system_uuid=>create_uuid_x16_static(  ).
      url = |{ ewaylink }|.
      client = create_client( url ).

      req = client->get_http_request(  ).
      req->set_header_field( i_name = 'gstin'    i_value = GSTIN  ).
      req->set_header_field( i_name = 'X-Cleartax-Auth-Token'    i_value = token ).
      req->set_content_type( 'application/json' ).

      req->set_text( ejson ) .
      DATA: result911 TYPE xstring.
      result911 = client->execute( if_web_http_client=>post )->to_xstring(  ) .
      DATA(ls_data_xml) = cl_web_http_utility=>encode_x_base64( result911 ).
      RESULT12 = ls_data_xml.
      client->close(  )  .

   ENDIF.

else.

RESULT12 = zeway_print=>get_error( variable = vbeln1 ) .


endiF.

  ENDMETHOD.
ENDCLASS.

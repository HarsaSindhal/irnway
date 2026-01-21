class ZCL_YEINV_HTTP definition
  public

  create public .
public section.

class-data : pdf2 type string ,

 pdf_xstring TYPE xSTRING.

  interfaces IF_HTTP_SERVICE_EXTENSION .
      TYPES : BEGIN OF ty_print,
              print TYPE string,
            END OF ty_print.
    CLASS-DATA : it_print        TYPE TABLE OF ty_print .


protected section.

private section.

ENDCLASS.



CLASS ZCL_YEINV_HTTP IMPLEMENTATION.


  method IF_HTTP_SERVICE_EXTENSION~HANDLE_REQUEST.
  DATA(req) = request->get_form_fields(  ).

data invoice type CHAR10 .
data invoiceto type CHAR10 .
data invoice1 type C LENGTH 10 .
data invoice1to type C LENGTH 10 .
data invoicenofrom type C LENGTH 10 .
data invoicenoto type C LENGTH 10 .
data year type string .
data transpoter_name type string .
DATA IRNGENRATE TYPE STRING .
DATA Eway_generate TYPE STRING .
DATA form_generate TYPE STRING .
DATA Eway_Cancellation TYPE STRING .
DATA Irn_Cancellation TYPE STRING .
DATA Vehiclenumber TYPE STRING .
DATA transportdoc TYPE STRING .
DATA transportid TYPE STRING .
DATA extraprint TYPE STRING .
DATA JSON TYPE STRING .
data irn_name type string .
data DISTANCE type string .
DATA companycode TYPE CHAR4 .
DATA ewaybillprint TYPE CHAR4 .
data type TYPE string.
data mail TYPE string.

invoice1           = value #( req[ name = 'invoice' ]-value optional ) .
invoice1to         = value #( req[ name = 'invoice2' ]-value optional ) .
invoicenofrom         = value #( req[ name = 'invoicenofrom' ]-value optional ) .
invoicenoto         = value #( req[ name = 'invoicenoto' ]-value optional ) .

if invoice1 is iniTIAL .
invoice1 = invoicenofrom .
endIF.

if invoice1to is iniTIAL .
invoice1to = invoicenoto .
endIF.

invoice            = |{ invoice1 ALPHA = IN }| .
invoiceto          = |{ invoice1to ALPHA = IN }| .
year               = value #( req[ name = 'fiscalyear' ]-value optional ) .
IRNGENRATE         = value #( req[ name = 'irn' ]-value optional ) .
Eway_generate      = value #( req[ name = 'eway' ]-value optional ) .
Form_generate      = value #( req[ name = 'form' ]-value optional ) .
Transpoter_name    = value #( req[ name = 'transporter' ]-value optional ) .
DISTANCE           = value #( req[ name = 'distance' ]-value optional ) .
Eway_Cancellation  = value #( req[ name = 'caneway' ]-value optional ) .
Irn_Cancellation   = value #( req[ name = 'canirn' ]-value optional ) .
Vehiclenumber      = value #( req[ name = 'vehiclenumber' ]-value optional ) .
Transportid        =  value #( req[ name = 'transportid' ]-value optional ) .
Transportdoc       =  value #( req[ name = 'transportdoc' ]-value optional ) .
JSON               =  value #( req[ name = 'json' ]-value optional ) .
companycode        =  value #( req[ name = 'companycode' ]-value optional ) .
type               =  value #( req[ name = 'type' ]-value optional ) .
mail               =  value #( req[ name = 'mail' ]-value optional ) .
extraprint          =  value #( req[ name = 'extraprint' ]-value optional ) .
ewaybillprint      =  value #( req[ name = 'ewaybillprint' ]-value optional ) .
data(invtype)      =  value #( req[ name = 'invoicetype' ]-value optional ) .
data(invoiceMulti)      =  value #( req[ name = 'invoice1' ]-value optional ) . "Sales Invoice

data(financeinvoice1)      =  value #( req[ name = 'financeinvoice1' ]-value optional ) . "Finance Invoice

    it_print = VALUE #( ( print = 'Original For Recipient ' ) ( print = 'Duplicate For Transporter' ) ( print = 'Triplicate For Supplier ' ) ) .

**********************Purchase Return************************************************
IF invtype = 'Purchase Return'.

data accountingdoc  TYPE CHAR14.
accountingdoc = VALUE #( req[ name = 'accountingdocument' ]-value OPTIONAL ) .
DATA companycode1 TYPE CHAR4  .
companycode1 =  value #( req[ name = 'companycode' ]-value optional ) .
if Transportid   is INITIAL .
Transportid  =  value #( req[ name = 'transid' ]-value optional ) .
ENDIF.
if Transportdoc   is INITIAL .
Transportdoc =  value #( req[ name = 'transdocno' ]-value optional ) .
ENDIF.
ENDIF.
*****************************Purchase Return*****************************************

IF invtype =  'Sales Invoice' AND json <> 'X' AND Form_generate <> 'X' AND Irn_Cancellation <> 'X' AND Eway_Cancellation <> 'X' AND mail <> 'X' .


DATA REVIEW TYPE STRING.
DATA REVIEWMulit TYPE STRING.
    SELECT FROM i_billingdocument AS a
    FIELDS
    a~billingdocument ,
    a~billingdocumenttype ,
    a~salesorganization ,
    a~distributionchannel ,
    a~division ,
    a~AccountingDocument,
    a~BillingDocumentIsCancelled , a~FiscalYear
     WHERE a~billingdocument = @invoice
     AND CompanyCode = @companycode
    INTO TABLE @DATA(it_acc).

if sy-subrc <> 0 .

review = 'Error Please input correct details'.

else .
 SORT it_acc BY BillingDocument.
 DELETE ADJACENT DUPLICATES FROM it_acc COMPARING BillingDocument.

LOOP AT it_acc INTO DATA(WA_MULTI).

CLEAR:invoice,invoice1.

invoice  = WA_MULTI-BillingDocument.
invoice1 =  WA_MULTI-BillingDocument.

if  ( IRNGENRATE = 'X'  OR ( Eway_generate = 'X' AND WA_MULTI-BillingDocumentType <> 'F8' AND WA_MULTI-BillingDocumentType <> 'JSN' AND WA_MULTI-BillingDocumentType <> 'JSP' ) ) .

SELECT * FROM I_BillingDocumentItem WITH PRIVILEGED ACCESS
 WHERE BillingDocument = @invoice1 AND CompanyCode = @companycode
 AND BillingQuantity <> 0 INTO TABLE @DATA(BILLDATA) .
 if WA_MULTI-DistributionChannel <> '17' .

 READ TABLE BILLDATA INTO data(nontax) INDEX  1.

 if  nontax-PRODUCTTAXCLASSIFICATION1 = '0' and nontax-ProductTaxClassification2 = '0' and nontax-ProductTaxClassification3 = '0'
 and nontax-ProductTaxClassification4 = '0' and nontax-ProductTaxClassification5 = '0' and nontax-ProductTaxClassification6 = '0' .

LOOP AT BILLDATA INTO DATA(WA_BILLDTA) .


SELECT SINGLE conditionbaseamount FROM i_billingdocumentitemprcgelmnt WITH PRIVILEGED ACCESS WHERE billingdocument = @WA_BILLDTA-billingdocument
                                          AND billingdocumentitem = @WA_BILLDTA-billingdocumentitem  AND
conditionbaseamount IS NOT INITIAL AND  conditiontype IN ( 'JOIG' ,'JOCG' ,'JOSG' ) INTO  @DATA(GST) .

IF GST = 0 .
REVIEWMulit = 'Error: Please Maintain GST.' &&  WA_BILLDTA-billingdocument  &&  WA_BILLDTA-BillingDocumentItem .
*EXIT.
ENDIF.

CLEAR:GST,WA_BILLDTA.

ENDLOOP.
ENDIF.
ENDIF.

IF REVIEWMulit = ' ' .

IF WA_MULTI-BillingDocumentIsCancelled <> 'X'  AND WA_MULTI-AccountingDocument <> '' .

SELECT SINGLE * FROM Y1ig_invrefnum WHERE bukrs = @companycode AND docno = @invoice INTO @DATA(IRNCHECHK).
SELECT SINGLE * FROM YJ1IG_EWAYBILLDD WHERE bukrs = @companycode AND docno = @invoice INTO @DATA(EWAYCHECHK2).

IF IRNCHECHK IS INITIAL OR ( Eway_generate = 'X' AND IRNGENRATE <> 'X' ).

REVIEWMulit  =  yeinvoice_te=>get_table_fields(    invoice =  invoice          companycode = companycode
                                                    irngenrate = IRNGENRATE     Eway_generate = Eway_generate
                                                    distance = DISTANCE         Transportid = Transportid
                                                    transportdoc = Transportdoc Vehiclenumber = Vehiclenumber
                                                    transpoter_name =  transpoter_name   ) .
ELSE.
REVIEWMulit =    |Error This Bill Is { invoice1  } **** Duplicate IRN request. This IRN is already generated  | .
ENDIF.
ELSEIF WA_MULTI-BillingDocumentIsCancelled = 'X'  OR WA_MULTI-AccountingDocument = '' .

IF WA_MULTI-BillingDocumentIsCancelled = 'X' .
     REVIEWMulit =    |This Bill Is { invoice1  } **** Cancel  | .
ELSEIF   WA_MULTI-AccountingDocument = '' .
     REVIEWMulit =    |Error This Bill { invoice1  } Against Not Accounting Hit. Please Bill Accounting Check**** | .
ENDIF.

ENDIF.
ENDIF.


ELSEIF  Eway_generate = 'X' AND invtype = 'Sales Invoice' AND ( WA_MULTI-BillingDocumentType = 'JSN' OR WA_MULTI-BillingDocumentType = 'F8' OR WA_MULTI-BillingDocumentType = 'F2' ).

   SELECT SINGLE * FROM YJ1IG_EWAYBILLDD WHERE bukrs = @companycode AND docno = @invoice AND Status <> 'C' INTO @DATA(EWAYCHECHK).

   IF EWAYCHECHK IS INITIAL .

   REVIEWMulit = yeway_bill_without_irn=>generated_eway_bill( invoice   = invoice companycode = companycode
                                  distance        = DISTANCE       Eway_generate = Eway_generate
                                  transportdoc    = transportdoc
                                  transportid     = transportid
                                  vehiclenumber   = Vehiclenumber
                                  transpoter_name = transpoter_name )  .
  ELSE.
  REVIEWMulit =    |Error This Bill Is { invoice1  } **** Duplicate E-WAY request. This E-WAY is already generated  | .
  ENDIF.


ENDIF.

REVIEW = REVIEW && cl_abap_char_utilities=>cr_lf && REVIEWMulit .
CLEAR:WA_MULTI,REVIEWMulit.

ENDLOOP.

ENDIF.
ENDIF.

IF ( IRNGENRATE = 'X'  AND invtype = 'Finance Invoice' ) .


**********************************************************************

      select AccountingDocument , FiscalYear , CompanyCode from I_OperationalAcctgDocItem
      where AccountingDocument = @invoice AND CompanyCode = @companycode AND FiscalYear = @year into table  @data(billhead_fi_co).

SORT billhead_fi_co  BY AccountingDocument .
DELETE ADJACENT DUPLICATES FROM billhead_fi_co COMPARING  AccountingDocument  .
**********************************************************************

 loop at billhead_fi_co INTO data(invdata_fi_co)  .
 invoice = invdata_fi_co-AccountingDocument.
 REVIEWMulit = zfi_e_invoice=>get_table_fields( invoice = invoice year = year irngenrate = IRNGENRATE  companycode = companycode  ) .
REVIEW = REVIEW && cl_abap_char_utilities=>cr_lf && REVIEWMulit .
CLEAR:invdata_fi_co,REVIEWMulit, invoice.
ENDLOOP.

********************************Purchase Return**************************************
ELSEIF (  Eway_generate = 'X' AND  invtype = 'Purchase Return' ).
IF invoice IS INITIAL .
invoice = accountingdoc.
ENDIF.

IF accountingdoc IS INITIAL .
accountingdoc = invoice.
ENDIF.
          review = zeway_bill_pur_return=>get_table_fields(     invoice            = invoice
                                                                companycode        = companycode
                                                                year               = year
                                                                Eway_generate      = Eway_generate
                                                                invoice1           = accountingdoc
                                                                intype             = invtype
                                                                distance           = distance
                                                                transpoter_name    = transpoter_name
                                                                transportid        = transportid
                                                                transportdoc       = transportdoc
                                                                vehiclenumber      = vehiclenumber

                                          ).
************************************END Purchase Return**********************************
elseIF  Form_generate EQ 'X' .

*********************************************************FORM SD  ************************************
if invtype = 'Sales Invoice' or type = 'Invoice and E-Way Bill Print'.


IF invoiceto IS INITIAL .
invoiceto = invoice.
ENDIF.

select * from I_BillingDocumentBasic where BillingDocument BETWEEN @invoice AND @invoiceto "AND CompanyCode = @companycode
into table  @data(billhead)  .

SORT billhead  BY BillingDocument .

if sy-subrc <> 0 .

review = 'Error Please input correct details'.

else .

if lines( billhead ) le 30 .

DATA(l_merger) = cl_rspo_pdf_merger=>create_instance( ).

loop at billhead INTO data(invdata)  .

SELECT SINGLE PLANT FROM I_BillingDocumentItem WHERE BillingDocument = @invdata-BillingDocument AND CompanyCode = @companycode INTO @DATA(plant).

invoice   =  invdata-BillingDocument .

if extraprint = 'Y'.

 pdf2 = zsd_dom_form=>read_posts( variable = invoice  variable1 = invoice  print = 'Extra Copy' ) . "printtype = printtype  textcopy = textcopy ) .
 pdf_xstring = xco_cp=>string( pdf2 )->as_xstring( xco_cp_binary=>text_encoding->base64 )->value.
 l_merger->add_document( pdf_xstring ).

ELSEIF type = 'EwayBillPrint'.

pdf2 = zeway_print=>read_posts( variable = invoice  ) . "printtype = printtype  textcopy = textcopy ) .
pdf_xstring = xco_cp=>string( pdf2 )->as_xstring( xco_cp_binary=>text_encoding->base64 )->value.
l_merger->add_document( pdf_xstring ).

elsEIF type = 'Invoice and E-Way Bill Print' .

 LOOP AT it_print INTO DATA(wa_print2) .

 pdf2 = zsd_dom_form=>read_posts( variable = invoice  variable1 = invoice  print = wa_print2-print ) . "printtype = printtype  textcopy = textcopy ) .
 pdf_xstring = xco_cp=>string( pdf2 )->as_xstring( xco_cp_binary=>text_encoding->base64 )->value.
 l_merger->add_document( pdf_xstring ).

 ENDLOOP.

pdf2 = zeway_print=>read_posts( variable = invoice  ) . "printtype = printtype  textcopy = textcopy ) .
pdf_xstring = xco_cp=>string( pdf2 )->as_xstring( xco_cp_binary=>text_encoding->base64 )->value.
l_merger->add_document( pdf_xstring ).


else.

  LOOP AT it_print INTO DATA(wa_print) .

 pdf2 = zsd_dom_form=>read_posts( variable = invoice  variable1 = invoice  print = wa_print-print ) . "printtype = printtype  textcopy = textcopy ) .
 pdf_xstring = xco_cp=>string( pdf2 )->as_xstring( xco_cp_binary=>text_encoding->base64 )->value.
 l_merger->add_document( pdf_xstring ).

ENDLOOP.

ENDIF.

clear : invoice.
clear : invdata.
ENDLOOP.

 TRY .
    DATA(l_poczone_PDF) = l_merger->merge_documents( ).
      CATCH cx_rspo_pdf_merger INTO DATA(l_exception).
        " Add a useful error handling here
    ENDTRY.
        DATA(response_final) = xco_cp=>xstring( l_poczone_PDF
      )->as_string( xco_cp_binary=>text_encoding->base64
      )->value .

 review = response_final.

else .

  review = 'Error Please Select Maximum 30 Document' .

ENDIF.
ENDIF.



ELSEIF invtype = 'Finance Invoice'.
*********************************************************FORM SD ************************************

*********************************************************FORM FINANCE ************************************
      select AccountingDocument , FiscalYear , CompanyCode from I_OperationalAcctgDocItem
      where AccountingDocument = @invoice AND CompanyCode = @companycode AND FiscalYear = @year into table  @data(billhead_fi).

SORT billhead_fi  BY AccountingDocument .
DELETE ADJACENT DUPLICATES FROM billhead_fi COMPARING  AccountingDocument  .
**********************************************************************

                      if sy-subrc <> 0 .
                         review = 'Error Please input correct details'.
                      else .

         if lines( billhead_fi ) le 30 .
          l_merger = cl_rspo_pdf_merger=>create_instance( ).

 loop at billhead_fi INTO data(invdata_fi)  .
  invoice   =  invdata_fi-AccountingDocument .

      pdf2 = z_fb70_invoice=>read_posts( year = year docno = invoice comcode = companycode ) .
      review = pdf2.
 pdf_xstring = xco_cp=>string( pdf2 )->as_xstring( xco_cp_binary=>text_encoding->base64 )->value.
 l_merger->add_document( pdf_xstring ).

ENDLOOP.

 TRY .
    l_poczone_PDF = l_merger->merge_documents( ).
      CATCH cx_rspo_pdf_merger INTO l_exception.
        " Add a useful error handling here
    ENDTRY.
        response_final = xco_cp=>xstring( l_poczone_PDF
      )->as_string( xco_cp_binary=>text_encoding->base64
      )->value .

 review = response_final.

else .

  review = 'Error Please Select Maximum 30 Document' .

ENDIF.
ENDIF.



endif .
*********************************************************FORM FINANCE ************************************
ELSEIF  Irn_Cancellation = 'X' OR Eway_Cancellation = 'X'  .

  if invtype = 'Sales Invoice'.

   DATA invvoice TYPE string .
IF Irn_Cancellation = 'X' .
  DATA(REVIEW1)  =  zsd_irn_cancel=>cancel_irn( invvoice = invoice companycode = companycode  cancelirn = irn_cancellation ) .
 ELSEIF Eway_Cancellation = 'X' .                                          "
  REVIEW1  =  zsd_irn_cancel=>EWAY_BILL_CANCEL( invvoice = invoice companycode = companycode  canceleway_bill = eway_cancellation ) .
 ENDIF.
 review = review1.

 ELSEIF invtype = 'Finance Invoice' .

  REVIEW1  =  zsd_irn_cancel=>cancel_irn( invVoice = invoice companycode = companycode  invtype = invtype cancelirn = irn_cancellation year = year ) .
  review1  = |PART 1{ REVIEW1 } | .
  review   = review1.

ENDIF.

elseif json = 'X'.
*********************************************************JSON ************************************
IF invtype = 'Sales Invoice' .

    SELECT SINGLE     a~billingdocument ,
    a~billingdocumenttype ,
    a~salesorganization ,
    a~distributionchannel ,
    a~division ,
    a~AccountingDocument,
    a~BillingDocumentIsCancelled , a~FiscalYear
     FROM i_billingdocument AS a
     WHERE a~billingdocument = @invoice
     AND CompanyCode = @companycode
    INTO  @DATA(DEV_t_acc).


 data(bing)  =  zsd_invoice_irn_data=>get_table_fields(       invoice =  invoice
                                                    companycode = companycode
                                                    irngenrate = IRNGENRATE
                                                 Eway_generate = Eway_generate
                                                      distance = DISTANCE
                                                   Transportid = Transportid
                                                  transportdoc = Transportdoc
                                                    Vehiclenumber = Vehiclenumber
                                                transpoter_name =  transpoter_name   )    .


ELSEIF invtype = 'Finance Invoice' .
  BING    =  zfi_invoice_irn_data=>get_table_fields( year = year invoice = invoice companycode = companycode ) .

ELSEIF invtype = 'Purchase Return'.

IF invoice IS INITIAL .
invoice = accountingdoc.
ENDIF.

IF accountingdoc IS INITIAL .
accountingdoc = invoice.
ENDIF.

    bing = zpur_return_invoice_irn_data=>generated_eway_bill( invoice = invoice year = year companycode = companycode invoice1 = accountingdoc
    transportdoc = transportdoc transportid = transportid transpoter_name = transpoter_name vehiclenumber = vehiclenumber distance = distance ) .


ENDIF.

 review = bing.


elseif type = 'Mail'.

*if invoicenoto is inITIAL .
*invoicenoto = invoicenofrom.
*endiF.
*
*    SELECT FROM i_billingdocument AS a
*    FIELDS
*    a~billingdocument ,
*    a~billingdocumenttype ,
*    a~salesorganization ,
*    a~distributionchannel ,
*    a~division ,
*    a~AccountingDocument,
*    a~BillingDocumentIsCancelled , a~FiscalYear
*     WHERE a~BillingDocumentIsCancelled <> 'X'
*     and a~billingdocument bETWEEN @invoicenofrom and @invoicenoto
*     INTO TABLE @DATA(it).
*
*lOoP aT it inTO data(wa).
*
*data inv tyPE string.
*inv = wa-BillingDocument.
*
*TRY.
*data(lv)  = zinv_auto_mail=>read_data( variable = inv ).
*CATCH cx_static_check.
*      "handle exception
*ENDTRY.
*endLOOP.
*********************************************************JSON **************************************
endif .


 response->set_text( review  ).

  endmethod.
ENDCLASS.

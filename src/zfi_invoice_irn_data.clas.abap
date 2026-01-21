CLASS zfi_invoice_irn_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
   INTERFACES if_oo_adt_classrun .
    CLASS-DATA : distan TYPE string .
    "CLASS FOR IRN GENERATION
     CLASS-METHODS :
       get_table_fields
        IMPORTING VALUE(invoice)     TYPE CHAR10
                  VALUE(companycode) TYPE CHAR4
                  VALUE(year)        TYPE string
        RETURNING VALUE(result)     TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZFI_INVOICE_IRN_DATA IMPLEMENTATION.


 METHOD get_table_fields.

    DATA itemlist TYPE TABLE OF yei_itemlist_tt .
    DATA wa_itemlist1 TYPE yei_itemlist_tt .
    DATA : it_data2 TYPE STANDARD TABLE OF yinv_stu,
           wa_data2 TYPE  yinv_stu.
    DATA: it_json_data TYPE TABLE OF  yqwerty4,
          wa_json_data TYPE yqwerty4,
          trandtls     TYPE yei_trandtls_tt,
          docdtls      TYPE yei_docdtls_st,
          sellerdtls   TYPE yei_sellerdtls_tt,
          buyerdtls    TYPE yei_buyerdtls_tt,
          dispdtls     TYPE yei_dispdtls_t1,
          shipdtls     TYPE yei_shipdtls_tt,
          it_itemlist  TYPE TABLE OF yeiv_itemlist_tt,
          wa_itemlist  TYPE yei_itemlist_tt,
          bchdtls      TYPE yei_bchdtls_tt,
          attribdtls   TYPE yei_attribdtls_tt,
          valdtls      TYPE yei_valdtls_tt,
          paydtls      TYPE yei_paydtls_tt,
          refdtls      TYPE yei_refdtls_tt,
          docpre       TYPE yei_docperddt_st,
          precdocdtls  TYPE yei_precdocdtls_tt,
          contrdtls    TYPE yei_contrdtls_tt,
          addldocdtls  TYPE yei_addldocdtls_tt,
          expdtls      TYPE yei_expdtls_tt,
          ewbdtls      TYPE yei_ewbdtls_st.

    DATA : igstamt TYPE yinv_stu-igstamt.
    DATA : Totaligstamt TYPE yinv_stu-igstamt.
    DATA : cgstamt TYPE yinv_stu-igstamt.
    DATA : Totalcgstamt TYPE yinv_stu-igstamt.
    DATA : sgstamt TYPE yinv_stu-igstamt.
    DATA : Totalsgstamt TYPE yinv_stu-igstamt.
    DATA : othamount  TYPE     yinv_stu-igstamt .
***************************************************************************

    DATA: vbeln1 TYPE c LENGTH 10.
    data: companycode1  type c length 04.

    vbeln1        =    |{ invoice ALPHA = IN }|.
    companycode1  =    |{ companycode  ALPHA = IN }|.


 select * from i_operationalacctgdocitem
   where accountingdocument = @vbeln1 AND TaxItemAcctgDocItemRef is not INITIAL
    and AccountingDocumentItemType <> 'T' AND CompanyCode = @companycode AND FiscalYear = @year
    and TaxCode <> 'A0' into table @data(i_billingpart2).

  loop at i_billingpart2 into data(wa_bill).
   data(wabill1) = wa_bill.
  endloop.

data(hsn_sac) = wa_bill-IN_HSNOrSACCode.
"""""""""""""""""""""""""""""""""""""""""""FETCHING PLANT FROM PROFIT CENTER"""""""
data plant1 type C LENGTH 4.
data var2   type C LENGTH 6.
 data(g_l2) = strlen( WABILL1-ProfitCenter ).
 var2       = WABILL1-ProfitCenter+6(4).
 plant1     = var2.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

SELECT * FROM i_operationalacctgdocitem
  where accountingdocument = @vbeln1 AND TaxItemAcctgDocItemRef is not INITIAL and AccountingDocumentItemType <> 'T'
  and TaxCode <> 'A0'
   AND CompanyCode = @companycode AND FiscalYear = @year into table @data(i_billingpart).

*SELECT * FROM YGSTR1_N_B2CL where accountingdocument = @vbeln1 and FiscalYear = @fisc and CompanyCode = @comp
*  APPENDING CORRESPONDING FIELDS OF TABLE @i_billingpart. """"""B2C

SELECT single * FROM ZSD_PLANT_ADDRESS
  where Plant = @plant1 into @data(plantaddress) .

SELECT SINGLE *  FROM i_operationalacctgdocitem  WHERE accountingdocument = @vbeln1
   and TaxItemAcctgDocItemRef is not INITIAL and AccountingDocumentItemType <> 'T' and TaxCode <> 'A0'
    AND CompanyCode = @companycode AND FiscalYear = @year INTO   @DATA(i_billingpart1).

SELECT SINGLE * FROM i_operationalacctgdocitem  WHERE accountingdocument = @vbeln1
   AND ( AccountingDocumentType = 'DR' OR AccountingDocumentType = 'DG' )
    AND CompanyCode = @companycode AND FiscalYear = @year INTO @DATA(maintab).

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
 IF i_billingpart1-customer IS INITIAL.
   SELECT SINGLE CUSTOMER FROM i_operationalacctgdocitem  WHERE accountingdocument = @vbeln1
                 and AccountingDocumentItemType <> 'T' and Customer <> ''
                  AND CompanyCode = @companycode AND FiscalYear = @year INTO  @DATA(CUST).
  DATA(CUST1) = CUST.

 ELSE .

 CUST1 = i_billingpart1-customer.

 ENDIF.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""Billing partner "BUYER'
    SELECT SINGLE * FROM i_customer WHERE Customer = @CUST1
    INTO @DATA(buyeradd).

    wa_data2-taxsch     =  'GST'.
    wa_data2-version    =  '1.1'.
    wa_data2-irn        =  'GST'.
    wa_data2-suptyp     =  'B2B'.
    wa_data2-regrev     =  'N'.
    wa_data2-ecmgstin   =  ' '.

    if buyeradd-taxnumber3 is initial.

    wa_data2-taxsch     =  'GST'.
    wa_data2-version    =  '1.1'.
    wa_data2-irn        =  'GST'.
    wa_data2-suptyp     =  'B2C'.
    wa_data2-regrev     =  'N'.
    wa_data2-ecmgstin   =  ' '.

    endif.

""""""""""""BUYER DETAILS""""""""""""""""""""""""""""""""""
    wa_data2-b_state    =  buyeradd-region.
    wa_data2-b_pos      =  buyeradd-region.
    wa_data2-b_pin      =  buyeradd-postalcode.

    wa_data2-i_no       =  vbeln1 .
    SHIFT wa_data2-i_no  LEFT DELETING LEADING '0'.
    wa_data2-i_dt       =  i_billingpart1-PostingDate.
    wa_data2-totinvval  =  ' ' .

""""""""""SUPPLIER DETAILS"""""""""""""""""""""""""""""""""
SELECT SINGLE * FROM  zship_add WHERE Plant = @Plant1 INTO @DATA(plant_add)  .

 SELECT single * FROM ZSD_PLANT_ADDRESS
 where  plant = @Plant1 into @data(sellerplantaddress) .

* IF Plant1 = '3201'.
* DATA(gsT1) = '08AAMCS8050K1ZT' .
* ENDIF.

if Plant1 = '1100' or Plant1 = '1110' or Plant1 = '1120' or Plant1 = '1130'
or Plant1 = '1140' or Plant1 = '1210' or Plant1 = '1220' or Plant1 = '1230'.
DATA(gst1) = '08AAGCV0492E1ZB'.
ELSEIF Plant1 = '1000'.
gst1 = '23AAGCV0492E1ZJ'.
ELSEIF Plant1 = '2100'.
gst1 = '08AEQPG8635R3ZF'.
ELSEIF Plant1 = '3100'.
gst1 = '23AAGCV4391L1ZY'.
ELSEIF Plant1 = '3200'.
gst1 = '33AAGCV4391L1ZX'.
ELSEIF Plant1 = '3300'.
gst1 = '29AAGCV4391L1ZM'.
ENDIF.

 if SY-SYSID = 'ALU' OR SY-SYSID = 'AWL'.

    wa_data2-s_gstin    =   '08AAFCD5862R018' .
    wa_data2-s_lglnm    =  plant_add-AddresseeFullName.
    wa_data2-s_trdnm    =  plant_add-AddresseeFullName.
    wa_data2-s_addr1    =  | { plant_add-StreetName } { plant_add-StreetPrefixName1 }  { plant_add-StreetPrefixName2 } |.
    wa_data2-s_addr2    =  | { plant_add-StreetSuffixName1 }  { plant_add-StreetSuffixName2 } { plant_add-DistrictName }  | .
    wa_data2-s_loc      =  plant_add-CityName.
    wa_data2-s_state    =  plant_add-Region .
    wa_data2-s_pin      =  plant_add-PostalCode.
    wa_data2-s_ph       =  ' '  .
    wa_data2-s_em       =  ' '  .

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
ELSE.


    wa_data2-s_gstin    = gsT1." '08AADCT0094P1ZV' . "gsT1.
    wa_data2-s_lglnm    =  sellerplantaddress-PlantName.
    wa_data2-s_trdnm    =  sellerplantaddress-PlantName.
    wa_data2-s_addr1    =  sellerplantaddress-AddresseeFullName .
    wa_data2-s_addr2    =  ' ' .
    wa_data2-s_loc      =  sellerplantaddress-city .
    wa_data2-s_state    =  sellerplantaddress-regcode.
    wa_data2-s_pin      =  sellerplantaddress-post .
    wa_data2-s_ph       =  ' '  .
    wa_data2-s_em       =  ' '  .
  ENDIF.



""""""""""Buyer details i_Customer details"""""""""""""""""

    IF buyeradd-country EQ 'IN' .
      wa_data2-b_gstin    =  buyeradd-taxnumber3.
    ELSE .
      wa_data2-b_gstin    =  'URP'.
    ENDIF .

    wa_data2-b_lglnm    =  buyeradd-customerfullname .
    wa_data2-b_trdnm    =  buyeradd-customerfullname .
    wa_data2-b_addr1    =  buyeradd-customerfullname .
    wa_data2-b_addr2    =  ' ' .
    wa_data2-b_loc      =  buyeradd-cityname .
    wa_data2-b_ph       =  buyeradd-faxnumber .
    wa_data2-b_em       =  ' ' .

""""""""""Dispatch details company address"""""""""""""""""

    wa_data2-d_nm       =  plantaddress-PlantName.
    wa_data2-d_addr1    =  plantaddress-AddresseeFullName.
    wa_data2-d_addr2    =  plantaddress-streete.
    wa_data2-d_loc      =  plantaddress-city.
    wa_data2-d_stcd     =  plantaddress-regcode.
    wa_data2-d_pin      =  plantaddress-post..

""""""""""Shiping Details  "Ship to party address"""""""""

      wa_data2-sh_gstin   =  buyeradd-taxnumber3 .
      wa_data2-sh_stcd    =  buyeradd-region .
      wa_data2-sh_pin     =  buyeradd-postalcode .
      wa_data2-sh_loc     =  buyeradd-cityname .
      wa_data2-sh_lglnm   =  buyeradd-customerfullname .
      wa_data2-sh_trdnm   =  buyeradd-customerfullname .
      wa_data2-sh_addr1   =  buyeradd-customerfullname .
      wa_data2-sh_addr2   = ' '  .



"""""""""""ITEM LIST"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT  i_billingpart INTO DATA(bill_item) .

   IF bill_item-AccountingDocumentType = 'DR' .
    wa_data2-i_typ = 'INV' .
    ELSEIF bill_item-AccountingDocumentType = 'DG' .
    wa_data2-i_typ = 'CRN' .
    ELSEIF bill_item-AccountingDocumentType = 'DN' .
    wa_data2-i_typ = 'DBN' .
    ENDIF.

    data slno type string.
    slno = slno + 1.

""""""""""""""""""""""""""""""""""FOR GST RATE"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
     SELECT SINGLE * FROM ztax_code_table WHERE taxcode = @bill_item-TaxCode  INTO @DATA(TAXRATE)  .
     IF TAXRATE-transactionkey = 'JIC' or TAXRATE-transactionkey = 'JIS' or TAXRATE-transactionkey = 'JOS'
     or TAXRATE-transactionkey = 'JOC'.
      wa_itemlist1-gstrt  = TAXRATE-gstrate * 2 .
     ELSE.
      wa_itemlist1-gstrt  =  TAXRATE-gstrate  .
     ENDIF.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      wa_itemlist1-slno       =  slno.
      wa_itemlist1-prddesc    =  bill_item-material .

SELECT SINGLE * FROM i_operationalacctgdocitem
  where accountingdocument = @vbeln1 AND IN_HSNOrSACCode is not INITIAL
   AND CompanyCode = @companycode AND FiscalYear = @year into @DATA(materai).


   if bill_item-IN_HSNOrSACCode is initial.
       bill_item-IN_HSNOrSACCode = materai-IN_HSNOrSACCode.
   endif.


      wa_itemlist1-hsncd      = bill_item-IN_HSNOrSACCode .

      if  wa_itemlist1-hsncd = '997212' or  wa_itemlist1-hsncd = '996111' or  wa_itemlist1-hsncd = '998821' .
      wa_itemlist1-isservc    =  'Y' .
      else.
      wa_itemlist1-isservc    =  'N' .
      ENDIF.

      wa_itemlist1-barcde     =  ' ' .
      wa_itemlist1-qty        =  '1.0'.
      wa_itemlist1-unit       =  'NOS'  .

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

   if bill_item-AmountInCompanyCodeCurrency LT 0 .
      wa_itemlist1-totamt     =  ( -1 * bill_item-AmountInCompanyCodeCurrency ).
     else.
      wa_itemlist1-totamt     =  bill_item-AmountInCompanyCodeCurrency.
      endif.

   if bill_item-AmountInCompanyCodeCurrency LT 0.
      wa_itemlist1-unitprice  =  ( -1 * bill_item-AmountInCompanyCodeCurrency ).
    else.
      wa_itemlist1-unitprice  =   bill_item-AmountInCompanyCodeCurrency .
     endif.

   if bill_item-AmountInCompanyCodeCurrency LT 0 .
      wa_itemlist1-assamt     =  ( -1 * bill_item-AmountInCompanyCodeCurrency ).
    else.
      wa_itemlist1-assamt     =  bill_item-AmountInCompanyCodeCurrency.
     endif.

"""""""""""""""""""""""""""""""""""""""""""""""TAX DATA*************************************************

select  single AmountInCompanyCodeCurrency from i_operationalacctgdocitem
where accountingdocument = @bill_item-AccountingDocument AND
                           TaxItemAcctgDocItemRef = @bill_item-TaxItemAcctgDocItemRef and
                           AccountingDocumentItemType = 'T'  AND CompanyCode = @companycode AND FiscalYear = @year and
                           TransactionTypeDetermination = 'JOI'
into  @data(igst1).

     if igst1 LT 0.
      igstamt  = igstamt + ( -1 * igst1 ).
      else.
      igstamt = igst1.
      endif.

     if igst1 LT 0.
      wa_itemlist1-igstamt    = ( -1 * igst1 ).
      else.
      wa_itemlist1-igstamt = igst1.
      endif.

 if igst1 is initial.

  select  single AmountInCompanyCodeCurrency from i_operationalacctgdocitem
   where accountingdocument = @bill_item-AccountingDocument
    AND TaxItemAcctgDocItemRef = @bill_item-TaxItemAcctgDocItemRef  AND CompanyCode = @companycode AND FiscalYear = @year
     and  AccountingDocumentItemType = 'T' and TransactionTypeDetermination = 'JOC'
      into  @data(cgst1).

 if cgst1 LT 0.
  cgstamt  = cgstamt +  ( -1 * cgst1 ).
  else.
  cgstamt = cgst1.
  endif.

 select single AmountInCompanyCodeCurrency from i_operationalacctgdocitem
  where accountingdocument = @bill_item-AccountingDocument
   AND TaxItemAcctgDocItemRef = @bill_item-TaxItemAcctgDocItemRef  AND CompanyCode = @companycode AND FiscalYear = @year
    and AccountingDocumentItemType = 'T' and TransactionTypeDetermination = 'JOS'
     into  @data(sgst1).

  if sgst1 LT 0.
   sgstamt  = sgstamt + ( -1 * sgst1 ).
   else.
   sgstamt = sgst1.
   endif.

   if sgst1 LT 0.
      wa_itemlist1-sgstamt  =  ( -1 * sgst1 ).
    else.
      wa_itemlist1-sgstamt = sgst1.
     endif.

   if cgst1 LT 0.
      wa_itemlist1-cgstamt  =  ( -1 * cgst1 ).
    else.
      wa_itemlist1-cgstamt  = cgst1.
     endif.

endif.

      wa_itemlist1-cesrt              =  '0'  .
      wa_itemlist1-cesamt             =  '0'  .
      wa_itemlist1-statecesrt         =  '0'  .
      wa_itemlist1-statecesamt        =  '0'  .
      wa_itemlist1-statecesnonadvlamt =  '0'  .

      othamount                       =  sgst1 + cgst1 + igst1 .
      wa_itemlist1-totitemval         =  wa_itemlist1-assamt   +  wa_itemlist1-igstamt + wa_itemlist1-cgstamt + wa_itemlist1-sgstamt .
      wa_itemlist1-ordlineref         =  ' '  .
      wa_itemlist1-orgcntry           =  ' '  .
      wa_itemlist1-prdslno            =  ' '  .

      DATA  assesmentval TYPE p DECIMALS 2 .
      assesmentval = assesmentval +  wa_itemlist1-totamt.
     Totaligstamt = Totaligstamt +  wa_itemlist1-igstamt.
     TotalCgstamt = TotalCgstamt +  wa_itemlist1-Cgstamt.
     TotalSgstamt = TotalSgstamt +  wa_itemlist1-Sgstamt.

      APPEND wa_itemlist1 TO itemlist .
      CLEAR :  wa_itemlist1,sgst1,cgst1,igst1 .

    ENDLOOP .

  select  single AmountInCompanyCodeCurrency,DebitCreditCode from i_operationalacctgdocitem
   where accountingdocument = @bill_item-AccountingDocument AND CompanyCode = @companycode AND FiscalYear = @year
   and GLAccount = '0003501006'
      into  @data(RONDOF).


""""""""""""""""""""AttribDtls Attribute"""""""""""""""""""""""""""""
    wa_data2-a_nm        =  ' '  .
    wa_data2-a_val       =  ' '  .

""""Batch details{ }
    wa_data2-b_nm        =  '12345' .
    wa_data2-b_expdt     =  ' '  .
    wa_data2-b_wrdt      =  ' '  .

""""""""""""""""""""ValDtls Value details{final} "ASSESENT LOGIC""""""
    wa_data2-assval      =   assesmentval  .
    wa_data2-cgstval     =   TotalCgstamt  .
    wa_data2-sgstval     =   TotalSgstamt  .
    wa_data2-igstval     =   Totaligstamt .
    wa_data2-cesval      =   '0.0'  .
    wa_data2-stcesval    =   '0.0'  .
    wa_data2-othchrg     =    othamount .
   wa_data2-rndoffamt    =   RONDOF-AmountInCompanyCodeCurrency  .

    IF RONDOF-DebitCreditCode = 'S'.
    RONDOF-AmountInCompanyCodeCurrency = RONDOF-AmountInCompanyCodeCurrency  * -1.
    ENDIF.
    wa_data2-totinvval   =    wa_data2-assval + TotalCgstamt + TotalSgstamt +  Totaligstamt + RONDOF-AmountInCompanyCodeCurrency .
    wa_data2-totinvvalfc =    wa_data2-assval + TotalCgstamt + TotalSgstamt +  Totaligstamt + RONDOF-AmountInCompanyCodeCurrency .


    "Payment Details
    wa_data2-p_nm       =  ' '  .
    wa_data2-p_mode     =  ' '  .
    wa_data2-p_fininsbr =  ' '  .
    wa_data2-p_payterm  =  ' '  .
    wa_data2-p_payinstr =  ' '  .
    wa_data2-p_crtrn    =  ' '  .
    wa_data2-p_dirdr    =  ' '  .
    wa_data2-p_crday    =  ' '  .
    wa_data2-p_paidamt  =  '0.0'.
    wa_data2-p_paymtdue =  '0.0'.
    wa_data2-p_accdet   =  ' '  .


    "RefDtls  refrence details  "Invoice remarks
    wa_data2-invrm      =   ' '  .

    "DocPerdDtls Perceding Details
    wa_data2-invstdt    =  '01/01/2022'  .
    wa_data2-invenddt   =  '01/01/2022'  .


    "PRECDocDtls
    wa_data2-p_invno    =   bill_item-accountingdocument   .
    wa_data2-p_invdt    = |{ bill_item-postingdate+6(2) }/{ bill_item-postingdate+4(2) }/{ bill_item-postingdate+0(4) } |    . "'20/09/2022'  .
    wa_data2-othrefno   =  ' '  .

    ""ContrDtls
    wa_data2-recadvrefr =  ' '  .
    wa_data2-recadvdt   =  ' '  .
    wa_data2-tendrefr   =  ' '  .
    wa_data2-contrrefr  =  ' '  .
    wa_data2-extrefr    =  ' '  .
    wa_data2-projrefr   =  ' '  .
    wa_data2-porefr     =  ' '  .
    wa_data2-porefdt    =  ' '  .

    "ADDLDocDtls
    wa_data2-url        =  ' '  .
    wa_data2-docs       =  ' '  .
    wa_data2-info       =  ' '  .

    "ExpDtls
    wa_data2-shipbno      =   ' '  .
    wa_data2-shipbdt      =   ' '  .
    wa_data2-port         =   ' '  .
    wa_data2-refclm       =   ' '  .
    wa_data2-totinvvalfc  =   ' ' .
    wa_data2-forcur       =    bill_item-transactioncurrency  .
    wa_data2-cntcode      =   ' ' .
    wa_data2-expduty      =   ' '  .

    "EwbDtls"
    wa_data2-transid    =   ' '  .
    wa_data2-transname  =   ' '  .
    wa_data2-transmode  =   '1'  .
    wa_data2-distance   =   ' '  .
    wa_data2-transdocno =   ' '  .
    wa_data2-transdocdt =   ' '  .
    wa_data2-vehno      =   ' ' .
    wa_data2-vehtype    =   'R'  .
    wa_data2-prctr      =   ' '  .
    wa_data2-distance1  =   ' '  .

    APPEND wa_data2  TO it_data2 .


    LOOP AT it_data2 INTO DATA(wa_final) .

      wa_json_data-version  =  wa_final-version .

*    IF bill_item-AccountingDocumentType = 'DR' .
*    docdtls-typ = 'INV' .
*    ELSEIF bill_item-AccountingDocumentType = 'DG' .
*    docdtls-typ = 'CRN' .
*    ELSEIF bill_item-AccountingDocumentType = 'DC'  OR bill_item-AccountingDocumentType = 'DN'.
*    docdtls-typ = 'DRN' .
*    ENDIF.

     docdtls-typ  = wa_final-i_typ .
     docdtls-no  =  wa_final-i_no .
     docdtls-dt  = zsd_invoice_irn_data=>dateformat( date = wa_final-i_dt )   .

"""""""Transport data

      trandtls-taxsch  = wa_final-taxsch  .
      trandtls-suptyp = wa_final-suptyp.
      trandtls-regrev = wa_final-regrev.
      trandtls-ecmgstin = wa_final-ecmgstin.


""""""""Seller_Data

      sellerdtls-gstin = wa_final-s_gstin.
      sellerdtls-lglnm = wa_final-s_lglnm.
      sellerdtls-trdnm = wa_final-s_trdnm.
      sellerdtls-addr1 = wa_final-s_addr1.
      REPLACE ALL OCCURRENCES OF ',' IN sellerdtls-addr1 WITH ' ' .
      REPLACE ALL OCCURRENCES OF '’' IN sellerdtls-addr1 WITH ' ' .
     "PERFORM format_data USING sellerdtls-addr1.
      sellerdtls-addr2 = wa_final-s_addr2.
      REPLACE ALL OCCURRENCES OF ',' IN sellerdtls-addr2 WITH ' ' .
      REPLACE ALL OCCURRENCES OF '’' IN sellerdtls-addr2 WITH ' ' .
      sellerdtls-loc = wa_final-s_loc.
      sellerdtls-pin = wa_final-s_pin.
      sellerdtls-stcd = wa_final-s_state.
      sellerdtls-ph = wa_final-s_ph .
      sellerdtls-em = wa_final-s_em .


""""""""Buyer details

      buyerdtls-gstin = wa_final-b_gstin.
      buyerdtls-lglnm = wa_final-b_lglnm.
      buyerdtls-trdnm = wa_final-b_trdnm.
      buyerdtls-pos = wa_final-b_pos.
      buyerdtls-addr1 = wa_final-b_addr1.
      REPLACE ALL OCCURRENCES OF ',' IN  buyerdtls-addr1 WITH ' ' .
      REPLACE ALL OCCURRENCES OF '’' IN  buyerdtls-addr1 WITH ' ' .
      buyerdtls-addr2 = wa_final-b_addr2.
      REPLACE ALL OCCURRENCES OF ',' IN  buyerdtls-addr2 WITH ' ' .
      REPLACE ALL OCCURRENCES OF '’' IN  buyerdtls-addr2 WITH ' ' .
      buyerdtls-loc = wa_final-b_loc .
      buyerdtls-pin = wa_final-b_pin .
      IF buyerdtls-pin = '0'.
        buyerdtls-pin = '111111'.
      ENDIF.
      buyerdtls-stcd = wa_final-b_state.
      buyerdtls-ph = wa_final-b_ph.
      buyerdtls-em = wa_final-b_em.


""""""""Dispatch details

      dispdtls-nm = wa_final-d_nm.
      dispdtls-addr1 = wa_final-d_addr1.
      REPLACE ALL OCCURRENCES OF ',' IN dispdtls-addr1 WITH ' ' .
      REPLACE ALL OCCURRENCES OF '’' IN dispdtls-addr1 WITH ' ' .
      dispdtls-addr2 = wa_final-d_addr2.
      REPLACE ALL OCCURRENCES OF ',' IN dispdtls-addr2 WITH ' ' .
      REPLACE ALL OCCURRENCES OF '’' IN dispdtls-addr2 WITH ' ' .
      dispdtls-loc = wa_final-d_loc.
      dispdtls-pin = wa_final-d_pin.
      dispdtls-stcd = wa_final-d_stcd.


""""""""shiping details

      shipdtls-gstin = wa_final-sh_gstin.
      shipdtls-lglnm = wa_final-sh_lglnm.
      shipdtls-trdnm = wa_final-sh_trdnm.
      shipdtls-addr1 = wa_final-sh_addr1.
      REPLACE ALL OCCURRENCES OF ',' IN shipdtls-addr1 WITH ' ' .
      REPLACE ALL OCCURRENCES OF '’' IN shipdtls-addr1 WITH ' ' .
      shipdtls-addr2 = wa_final-sh_addr2.
      REPLACE ALL OCCURRENCES OF ',' IN shipdtls-addr2 WITH ' ' .
      REPLACE ALL OCCURRENCES OF '’' IN shipdtls-addr2 WITH ' ' .
      shipdtls-loc = wa_final-sh_loc.
      shipdtls-pin = wa_final-sh_pin.
      shipdtls-stcd = wa_final-sh_stcd.



  LOOP AT itemlist INTO wa_itemlist  .

   APPEND wa_itemlist TO wa_json_data-itemlist .
    CLEAR wa_itemlist .

  ENDLOOP .

      attribdtls-nm = wa_final-a_nm.
      attribdtls-val = wa_final-a_val.
      DATA:assval TYPE string.
      DATA:inv    TYPE string.
      DATA:invfc  TYPE string.
      DATA:oth    TYPE string.
      DATA:sgst   TYPE string.
      DATA:cgst   TYPE string.
      DATA:igst   TYPE string.
      assval = assval + wa_final-assval.
      valdtls-assval = assval.   "1

      cgst   =  cgst + wa_final-cgstval.
      sgst   =  sgst + wa_final-sgstval.
      igst   =  igst + wa_final-igstval.

      valdtls-cgstval = cgst.
      valdtls-sgstval = sgst.
      valdtls-igstval = igst.

      valdtls-cesval    =  wa_final-cesval.
      valdtls-stcesval  =  wa_final-stcesval.
      valdtls-rndoffamt =  wa_final-rndoffamt.
*     valdtls-othchrg   =  wa_final-othchrg .

      invfc  =  invfc +  wa_final-totinvvalfc.

      valdtls-totinvvalfc = invfc .
      inv = inv + wa_final-totinvval .
      valdtls-totinvval = inv.

      paydtls-nm = wa_final-p_nm.
      paydtls-accdet = wa_final-p_accdet.
      paydtls-mode = wa_final-p_mode.
      paydtls-fininsbr = wa_final-p_fininsbr.
      paydtls-payterm = wa_final-p_payterm.
      paydtls-payinstr = wa_final-p_payinstr.
      paydtls-crtrn = wa_final-p_crtrn.
      paydtls-dirdr = wa_final-p_dirdr.
      paydtls-crday = wa_final-p_crday.
      paydtls-paidamt = wa_final-p_paidamt.
      paydtls-paymtdue = wa_final-p_paymtdue.

      refdtls-invrm = wa_final-invrm.

      docpre-invstdt = wa_final-invstdt.

      docpre-invenddt = wa_final-invenddt.
      precdocdtls-invno = wa_final-p_invno.
      precdocdtls-invdt = wa_final-p_invdt.
      precdocdtls-othrefno = wa_final-othrefno.

      contrdtls-recadvrefr = wa_final-recadvrefr.
      contrdtls-recadvdt = wa_final-recadvdt.
      contrdtls-tendrefr = wa_final-tendrefr.
      contrdtls-contrrefr = wa_final-contrrefr.
      contrdtls-extrefr = wa_final-extrefr.
      contrdtls-projrefr = wa_final-projrefr.
      contrdtls-porefr = wa_final-porefr.
      contrdtls-porefdt = wa_final-porefdt.


      addldocdtls-url = wa_final-url.
      addldocdtls-docs = wa_final-docs.


      expdtls-shipbno = wa_final-shipbno.
      expdtls-shipbdt = wa_final-shipbdt.
      expdtls-port = wa_final-port.
      expdtls-refclm = wa_final-refclm.
      expdtls-forcur = wa_final-forcur.
      expdtls-cntcode = wa_final-cntcode.


      bchdtls-nm = wa_final-b_nm.
      bchdtls-expdt = wa_final-b_expdt.
      bchdtls-wrdt = wa_final-b_wrdt.

      ewbdtls-transid = wa_final-transid.
      ewbdtls-transname = wa_final-transname.
      ewbdtls-transmode = wa_final-transmode.

      DATA: del1 TYPE string,
            del2 TYPE string.
      DATA: dest TYPE string.
      dest  =  wa_final-distance  .
      SPLIT dest AT '.' INTO del1 del2.
      ewbdtls-distance = del1."WA_FINAL-DISTANCE.
      IF del1 IS INITIAL.
        ewbdtls-distance = '0'.
      ENDIF.
      ewbdtls-transdocno = wa_final-transdocno.
      ewbdtls-transdocdt = wa_final-transdocdt.
      REPLACE ALL OCCURRENCES OF '.' IN ewbdtls-transdocdt WITH '/'.
      ewbdtls-vehno = wa_final-vehno.
      ewbdtls-vehtype = wa_final-vehtype.


      MOVE-CORRESPONDING trandtls TO wa_json_data-trandtls.
      MOVE-CORRESPONDING docdtls TO wa_json_data-docdtls.
      MOVE-CORRESPONDING sellerdtls TO wa_json_data-sellerdtls.
      MOVE-CORRESPONDING buyerdtls TO wa_json_data-buyerdtls.
      MOVE-CORRESPONDING dispdtls TO wa_json_data-dispdtls.
      MOVE-CORRESPONDING shipdtls TO wa_json_data-shipdtls.
      MOVE-CORRESPONDING attribdtls TO wa_json_data-attribdtls.
      MOVE-CORRESPONDING valdtls TO wa_json_data-valdtls.
      MOVE-CORRESPONDING paydtls TO wa_json_data-paydtls.
      MOVE-CORRESPONDING precdocdtls TO wa_json_data-precdocdtls.
      MOVE-CORRESPONDING contrdtls TO wa_json_data-contrdtls.
      MOVE-CORRESPONDING refdtls TO wa_json_data-refdtls.
      MOVE-CORRESPONDING docpre TO  wa_json_data-docpre.
      MOVE-CORRESPONDING addldocdtls TO wa_json_data-addldocdtls.
      MOVE-CORRESPONDING expdtls TO wa_json_data-expdtls.
      MOVE-CORRESPONDING bchdtls TO wa_json_data-bchdtls.
      MOVE-CORRESPONDING ewbdtls TO wa_json_data-ewbdtls.
      APPEND wa_json_data TO it_json_data.

      CLEAR: wa_final, trandtls, docdtls, sellerdtls, buyerdtls, dispdtls, shipdtls, wa_itemlist,
           bchdtls, valdtls, paydtls, refdtls, addldocdtls, expdtls, ewbdtls.


    ENDLOOP .

    DATA(json_writer) = cl_sxml_string_writer=>create(
                            type = if_sxml=>co_xt_json ).
    CALL TRANSFORMATION id SOURCE result = it_json_data
                           RESULT XML json_writer.
    DATA(jsonx) = json_writer->get_output( ).


    DATA: lv_json TYPE string.


    DATA :lv_xstring_var TYPE xstring,
          strc           TYPE string.

    DATA(lv_string) = xco_cp=>xstring( jsonx
      )->as_string( xco_cp_character=>code_page->utf_8
      )->value.

      REPLACE ALL OCCURRENCES OF 'VERSION' IN lv_string WITH 'Version'.
    REPLACE ALL OCCURRENCES OF 'IRN' IN lv_string WITH 'Irn'.
    REPLACE ALL OCCURRENCES OF 'TRANDTLS' IN lv_string WITH 'TranDtls'.
    REPLACE ALL OCCURRENCES OF 'DOCDTLS' IN lv_string WITH 'DocDtls'.
    REPLACE ALL OCCURRENCES OF 'SELLERDTLS' IN lv_string WITH 'SellerDtls'.
    REPLACE ALL OCCURRENCES OF 'BUYERDTLS' IN lv_string WITH 'BuyerDtls'.
    REPLACE ALL OCCURRENCES OF 'DISPDTLS' IN lv_string WITH 'DispDtls'.
    REPLACE ALL OCCURRENCES OF 'SHIPDTLS' IN lv_string WITH 'ShipDtls'.
    REPLACE ALL OCCURRENCES OF 'ITEMLIST' IN lv_string WITH 'ItemList'.
    REPLACE ALL OCCURRENCES OF 'VALDTLS' IN lv_string WITH 'ValDtls'.
    REPLACE ALL OCCURRENCES OF 'PAYDTLS' IN lv_string WITH 'PayDtls'.
    REPLACE ALL OCCURRENCES OF 'REFDTLS' IN lv_string WITH 'RefDtls'.
    REPLACE ALL OCCURRENCES OF 'ADDLDOCDTLS' IN lv_string WITH 'AddlDocDtls'.
    REPLACE ALL OCCURRENCES OF 'EXPDTLS' IN lv_string WITH 'ExpDtls'.
    REPLACE ALL OCCURRENCES OF 'EWBDTLS' IN lv_string WITH 'EwbDtls'.
    REPLACE ALL OCCURRENCES OF 'TAXSCH' IN lv_string WITH 'TaxSch'.
    REPLACE ALL OCCURRENCES OF 'SUPTYP' IN lv_string WITH 'SupTyp'.
    REPLACE ALL OCCURRENCES OF 'REGREV' IN lv_string WITH 'RegRev'.
    REPLACE ALL OCCURRENCES OF 'ECMGSTIN' IN lv_string WITH 'EcmGstin'.
    REPLACE ALL OCCURRENCES OF 'TYP' IN lv_string WITH 'Typ'.
    REPLACE ALL OCCURRENCES OF 'NO' IN lv_string WITH 'No'.
    REPLACE ALL OCCURRENCES OF 'DT' IN lv_string WITH 'Dt'.
    REPLACE ALL OCCURRENCES OF 'GSTIN' IN lv_string WITH 'Gstin'.
    REPLACE ALL OCCURRENCES OF 'LGLNM' IN lv_string WITH 'LglNm'.
    REPLACE ALL OCCURRENCES OF 'TRDNM' IN lv_string WITH 'TrdNm'.
    REPLACE ALL OCCURRENCES OF 'ADDR1' IN lv_string WITH 'Addr1'.
    REPLACE ALL OCCURRENCES OF 'ADDR2' IN lv_string WITH 'Addr2'.
    REPLACE ALL OCCURRENCES OF 'LOC' IN lv_string WITH 'Loc'.
    REPLACE ALL OCCURRENCES OF 'PIN' IN lv_string WITH 'Pin'.
    REPLACE ALL OCCURRENCES OF 'STATE' IN lv_string WITH 'State'.
    REPLACE ALL OCCURRENCES OF 'PH' IN lv_string WITH 'Ph'.
    REPLACE ALL OCCURRENCES OF 'EM' IN lv_string WITH 'Em'.
    REPLACE ALL OCCURRENCES OF 'POS' IN lv_string WITH 'Pos'.
    REPLACE ALL OCCURRENCES OF 'NM' IN lv_string WITH 'Nm'.
    REPLACE ALL OCCURRENCES OF 'STCD' IN lv_string WITH 'Stcd'.
    REPLACE ALL OCCURRENCES OF 'SLNO' IN lv_string WITH 'SlNo'.
    REPLACE ALL OCCURRENCES OF 'PRDDESC' IN lv_string WITH 'PrdDesc'.
    REPLACE ALL OCCURRENCES OF 'ISSERVC' IN lv_string WITH 'IsServc'.
    REPLACE ALL OCCURRENCES OF 'HSNCD' IN lv_string WITH 'HsnCd'.
    REPLACE ALL OCCURRENCES OF 'BCHDTLS' IN lv_string WITH 'BchDtls'.
    REPLACE ALL OCCURRENCES OF 'BARCDE' IN lv_string WITH 'Barcde'.
    REPLACE ALL OCCURRENCES OF 'QTY' IN lv_string WITH 'Qty'.
    REPLACE ALL OCCURRENCES OF 'FREEQTY' IN lv_string WITH 'FreeQty'.
    REPLACE ALL OCCURRENCES OF 'UNIT' IN lv_string WITH 'Unit'.
*  REPLACE ALL OCCURRENCES OF 'UNITPRICE' IN lv_string WITH 'UnitPrice'.
    REPLACE ALL OCCURRENCES OF 'TOTAMT' IN lv_string WITH 'TotAmt'.
    REPLACE ALL OCCURRENCES OF 'DISCOUNT' IN lv_string WITH 'Discount'.
    REPLACE ALL OCCURRENCES OF 'PRETAXVAL' IN lv_string WITH 'PreTaxVal'.
    REPLACE ALL OCCURRENCES OF 'ASSAMT' IN lv_string WITH 'AssAmt'.
    REPLACE ALL OCCURRENCES OF 'GSTRT' IN lv_string WITH 'GstRt'.
    REPLACE ALL OCCURRENCES OF 'IGSTAMT' IN lv_string WITH 'IgstAmt'.
    REPLACE ALL OCCURRENCES OF 'CGSTAMT' IN lv_string WITH 'CgstAmt'.
    REPLACE ALL OCCURRENCES OF 'SGSTAMT' IN lv_string WITH 'SgstAmt'.
    REPLACE ALL OCCURRENCES OF 'CESRT' IN lv_string WITH 'CesRt'.
    REPLACE ALL OCCURRENCES OF 'CESAMT' IN lv_string WITH 'CesAmt'.
    REPLACE ALL OCCURRENCES OF 'CESNONADVLAMT' IN lv_string WITH 'CesNonAdvlAmt'.
    REPLACE ALL OCCURRENCES OF 'STATECESRT' IN lv_string WITH 'StateCesRt'.
    REPLACE ALL OCCURRENCES OF 'STATECESAMT' IN lv_string WITH 'StateCesAmt'.
    REPLACE ALL OCCURRENCES OF 'STATECESNONADVLAMT' IN lv_string WITH 'StateCesNonAdvlAmt'.
    REPLACE ALL OCCURRENCES OF 'OTHCHRG' IN lv_string WITH 'OthChrg'.
    REPLACE ALL OCCURRENCES OF 'TOTITEMVAL' IN lv_string WITH 'TotItemVal'.
    REPLACE ALL OCCURRENCES OF 'ORDLINEREF' IN lv_string WITH 'OrdLineRef'.
    REPLACE ALL OCCURRENCES OF 'ORGCNTRY' IN lv_string WITH 'OrgCntry'.
    REPLACE ALL OCCURRENCES OF 'PRDSLNO' IN lv_string WITH 'PrdSlNo'.
    REPLACE ALL OCCURRENCES OF 'ATTRIBDTLS' IN lv_string WITH 'AttribDtls'.
    REPLACE ALL OCCURRENCES OF 'VAL' IN lv_string WITH 'Val'.
    REPLACE ALL OCCURRENCES OF 'EXPDT' IN lv_string WITH 'ExpDt'.
    REPLACE ALL OCCURRENCES OF 'WRDT' IN lv_string WITH 'WrDt'.
    REPLACE ALL OCCURRENCES OF 'ASSVAL' IN lv_string WITH 'AssVal'.
    REPLACE ALL OCCURRENCES OF 'CGSTVAL' IN lv_string WITH 'CgstVal'.
    REPLACE ALL OCCURRENCES OF 'SGSTVAL' IN lv_string WITH 'SgstVal'.
    REPLACE ALL OCCURRENCES OF 'IGSTVAL' IN lv_string WITH 'IgstVal'.
    REPLACE ALL OCCURRENCES OF 'CESVAL' IN lv_string WITH 'CesVal'.
    REPLACE ALL OCCURRENCES OF 'STCESVAL' IN lv_string WITH 'StCesVal'.
    REPLACE ALL OCCURRENCES OF 'RNDOFFAMT' IN lv_string WITH 'RndOffAmt'.
    REPLACE ALL OCCURRENCES OF 'TOTINVVALFC' IN lv_string WITH 'TotInvValFc'.
    REPLACE ALL OCCURRENCES OF 'ACCDET' IN lv_string WITH 'AccDet'.
    REPLACE ALL OCCURRENCES OF 'MODE' IN lv_string WITH 'Mode'.
    REPLACE ALL OCCURRENCES OF 'FININSBR' IN lv_string WITH 'FinInsBr'.
    REPLACE ALL OCCURRENCES OF 'PAYTERM' IN lv_string WITH 'PayTerm'.
    REPLACE ALL OCCURRENCES OF 'PAYINSTR' IN lv_string WITH 'PayInstr'.
    REPLACE ALL OCCURRENCES OF 'CRTRN' IN lv_string WITH 'CrTrn'.
    REPLACE ALL OCCURRENCES OF 'DIRDR' IN lv_string WITH 'DirDr'.
    REPLACE ALL OCCURRENCES OF 'CRDAY' IN lv_string WITH 'CrDay'.
    REPLACE ALL OCCURRENCES OF 'PAIDAMT' IN lv_string WITH 'PaidAmt'.
    REPLACE ALL OCCURRENCES OF 'PAYMTDUE' IN lv_string WITH 'PaymtDue'.
    REPLACE ALL OCCURRENCES OF 'INVRM' IN lv_string WITH 'InvRm'.
    REPLACE ALL OCCURRENCES OF 'INVSTDT' IN lv_string WITH 'InvStDt'.
    REPLACE ALL OCCURRENCES OF 'INVENDDTT' IN lv_string WITH 'InvEndDt'.
    REPLACE ALL OCCURRENCES OF 'PRECDOCDTLS' IN lv_string WITH 'PrecDocDtls'.
    REPLACE ALL OCCURRENCES OF 'CONTRDTLS' IN lv_string WITH 'ContrDtls'.
    REPLACE ALL OCCURRENCES OF 'INVNO' IN lv_string WITH 'InvNo'.
    REPLACE ALL OCCURRENCES OF 'INVDT' IN lv_string WITH 'InvDt'.
    REPLACE ALL OCCURRENCES OF 'OTHREFNO' IN lv_string WITH 'OthRefNo'.
    REPLACE ALL OCCURRENCES OF 'RECADVREFR' IN lv_string WITH 'RecAdvRefr'.
    REPLACE ALL OCCURRENCES OF 'RECADVDT' IN lv_string WITH 'RecAdvDt'.
    REPLACE ALL OCCURRENCES OF 'TENDREFR' IN lv_string WITH 'TendRefr'.
    REPLACE ALL OCCURRENCES OF 'CONTRREFR' IN lv_string WITH 'ContrRefr'.
    REPLACE ALL OCCURRENCES OF 'EXTREFR' IN lv_string WITH 'ExtRefr'.
    REPLACE ALL OCCURRENCES OF 'PROJREFR' IN lv_string WITH 'ProjRefr'.
    REPLACE ALL OCCURRENCES OF 'POREFR' IN lv_string WITH 'PORefr'.
    REPLACE ALL OCCURRENCES OF 'POREFDT' IN lv_string WITH 'PORefDt'.
    REPLACE ALL OCCURRENCES OF 'URL' IN lv_string WITH 'Url'.
    REPLACE ALL OCCURRENCES OF 'DOCS' IN lv_string WITH 'Docs'.
    REPLACE ALL OCCURRENCES OF 'INFO' IN lv_string WITH 'Info'.
    REPLACE ALL OCCURRENCES OF 'SHIPBNO' IN lv_string WITH 'ShipBNo'.
    REPLACE ALL OCCURRENCES OF 'SHIPBDT' IN lv_string WITH 'ShipBDt'.
    REPLACE ALL OCCURRENCES OF 'PORT' IN lv_string WITH 'Port'.
    REPLACE ALL OCCURRENCES OF 'REFCLM' IN lv_string WITH 'RefClm'.
    REPLACE ALL OCCURRENCES OF 'FORCUR' IN lv_string WITH 'ForCur'.
    REPLACE ALL OCCURRENCES OF 'CNTCODE' IN lv_string WITH 'CntCode'.
    REPLACE ALL OCCURRENCES OF 'TRANSID' IN lv_string WITH 'TransId'.
    REPLACE ALL OCCURRENCES OF 'TRANSNAME' IN lv_string WITH 'TransName'.
    REPLACE ALL OCCURRENCES OF 'TRANSMODE' IN lv_string WITH 'TransMode'.
    REPLACE ALL OCCURRENCES OF 'DISTANCE' IN lv_string WITH 'Distance'.
    REPLACE ALL OCCURRENCES OF 'TRANSDOCNO' IN lv_string WITH 'TransDocNo'.
    REPLACE ALL OCCURRENCES OF 'TRANSDOCDT' IN lv_string WITH 'TransDocDt'.
    REPLACE ALL OCCURRENCES OF 'VEHNO' IN lv_string WITH 'VehNo'.
    REPLACE ALL OCCURRENCES OF 'VEHTYPE' IN lv_string WITH 'VehType'.
    REPLACE ALL OCCURRENCES OF 'SLNo' IN lv_string WITH 'SlNo'.
    REPLACE ALL OCCURRENCES OF 'UnitPRICE' IN lv_string WITH 'UnitPrice'.
    REPLACE ALL OCCURRENCES OF 'TOTITEmVal' IN lv_string WITH 'TotItemVal'.
    REPLACE ALL OCCURRENCES OF 'TOTINVVal' IN lv_string WITH 'TotInvVal'.

    REPLACE ALL OCCURRENCES OF 'ASSVal' IN lv_string WITH 'AssVal'.
    REPLACE ALL OCCURRENCES OF 'CGSTVal' IN lv_string WITH 'CgstVal'.
    REPLACE ALL OCCURRENCES OF 'SGSTVal' IN lv_string WITH 'SgstVal'.
    REPLACE ALL OCCURRENCES OF 'IGSTVal' IN lv_string WITH 'IgstVal'.
    REPLACE ALL OCCURRENCES OF 'CESVal' IN lv_string WITH 'CESVal'.



    SHIFT lv_string LEFT DELETING LEADING '{"RESULT":'.
    REPLACE ALL OCCURRENCES OF '[{'  IN lv_string WITH '{'.
    REPLACE ALL OCCURRENCES OF '{'  IN lv_string WITH '{*'.
    REPLACE ALL OCCURRENCES OF ',"' IN lv_string WITH ',*"'.
    REPLACE ALL OCCURRENCES OF '}]' IN lv_string WITH '}'.
    REPLACE ALL OCCURRENCES OF '},' IN lv_string WITH '},*'.
    REPLACE ALL OCCURRENCES OF '}' IN lv_string WITH '*}'.
    REPLACE ALL OCCURRENCES OF '*}*}*}' IN lv_string WITH '*}*}'.
    REPLACE ALL OCCURRENCES OF 'ItemList":{' IN lv_string WITH 'ItemList":[{'.
    REPLACE ALL OCCURRENCES OF '},**"ATTRIBDtLS":{' IN lv_string WITH '},*"AttribDtls":[{'.
    REPLACE ALL OCCURRENCES OF '},**"ValDtls":' IN lv_string WITH '}]}],*"ValDtls":'.
    REPLACE ALL OCCURRENCES OF '"PRECDocDtls":' IN lv_string WITH '"PRECDocDtls":['.
    REPLACE ALL OCCURRENCES OF '},**"ADDLDocDtls":' IN lv_string WITH '}]},*"ADDLDocDtls":['.
    REPLACE ALL OCCURRENCES OF '},**"ExpDtls":' IN lv_string WITH '}],*"ExpDtls":'.
    REPLACE ALL OCCURRENCES OF '"CONTRDtLS":' IN lv_string WITH '"ContrDtls":['.
    REPLACE ALL OCCURRENCES OF '},**"BCHDtLS":' IN lv_string WITH ',*"BchDtls":'.
    REPLACE ALL OCCURRENCES OF '},**"DOCPRE":{' IN lv_string WITH ',*"DocPerdDtls":{'.
    REPLACE ALL OCCURRENCES OF '},**"ContrDtls":[{' IN lv_string WITH '}],*"ContrDtls":[{'.
    REPLACE ALL OCCURRENCES OF '**' IN lv_string WITH '*'.
    REPLACE ALL OCCURRENCES OF '"Distance": 0.0,' IN lv_string WITH '"Distance": 0,'.

    REPLACE ALL OCCURRENCES OF '""' IN lv_string WITH 'null'.
    REPLACE ALL OCCURRENCES OF '111111' IN lv_string WITH 'null'.

    REPLACE ALL OCCURRENCES OF '*' IN lv_string WITH ''.
     REPLACE ALL OCCURRENCES OF '07ADtPM7389J1ZJ' IN lv_string WITH '07ADTPM7389J1ZJ'.
     REPLACE ALL OCCURRENCES OF '29AEEPh1574G1ZJ' IN lv_string WITH '29AEEPH1574G1ZJ' RESPECTING CASE.

   lv_string = '[{"transaction":' &&   lv_string && '}]'.

    result = lv_string.
    ENDMETHOD.
ENDCLASS.

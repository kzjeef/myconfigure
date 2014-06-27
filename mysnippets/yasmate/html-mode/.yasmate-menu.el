;; Translated menu
;; 
(yas-define-menu 'html-mode
                 '(;; Documentation for Tag
                   (yas-item "637CEA2B-578C-429C-BB74-30E8D42BFA22")
                   ;; Ignoring Validate Syntax (W3C)
                   (yas-ignore-item "3F26240E-6E4A-11D9-B411-000D93589AF6")
                   
                   ;; Ignoring Show Web Preview
                   (yas-ignore-item "AC5F664E-86BA-4D81-B6CE-1B12F69FA490")
                   ;; Open Document in Running Browser(s)
                   (yas-item "970EE6B4-A091-11D9-A5A2-000D93C8BE28")
                   ;; Ignoring Refresh Running Browser(s)
                   (yas-ignore-item "B8651C6E-A05E-11D9-86AC-000D93C8BE28")
                   
                   (yas-submenu "Entities"
                                (;; Ignoring Convert Character / Selection to Entities
                                 (yas-ignore-item "3DD8406C-A116-11D9-A5A2-000D93C8BE28")
                                 ;; Ignoring Convert Character / Selection to Entities Excl. Tags
                                 (yas-ignore-item "43C9E8AE-3E53-4B82-A1AF-56697BB3EF09")
                                 ;; Ignoring Decode Entities in Line / Selection
                                 (yas-ignore-item "C183920D-A126-11D9-A5A2-000D93C8BE28")
                                 
                                 ;; Non-Breaking Space
                                 (yas-item "73B40BAE-A295-11D9-87F7-000D93C8BE28")
                                 (yas-separator)
                                 ;; Ignoring Insert Entity…
                                 (yas-ignore-item "89E5CC0A-3EFF-4DEF-A299-2E9651DE6529")))
                   (yas-submenu "URL Escapes"
                                (;; Ignoring URL Escape Line / Selection
                                 (yas-ignore-item "6B024865-6095-4CE3-8EDD-DC6F2230C2FF")
                                 ;; Ignoring URL Unescape Line / Selection
                                 (yas-ignore-item "2C4C9673-B166-432A-8938-75A5CA622481")))
                   ;; Ignoring Encrypt Line / Selection (ROT 13)
                   (yas-ignore-item "9B13543F-8356-443C-B6E7-D9259B604927")
                   
                   ;; Ignoring CodeCompletion HTML Attributes
                   (yas-ignore-item "CBD82CF3-74E9-4E7A-B3F6-9348754EB5AA")
                   ;; Insert Open/Close Tag (With Current Word)
                   (yas-item "2ED44A32-C353-447F-BAE4-E3522DB6944D")
                   ;; Ignoring Insert Close Tag
                   (yas-ignore-item "0658019F-3635-462E-AAC2-74E4FE508A9B")
                   (yas-submenu "Insert DocType"
                                (;; HTML — 5
                                 (yas-item "08E4F47C-A570-4F9B-A6AE-DCAC0D2E2420")
                                 (yas-separator)
                                 ;; HTML — 4.01 Strict
                                 (yas-item "944F1410-188C-4D70-8340-CECAA56FC7F2")
                                 ;; HTML — 4.01 Transitional
                                 (yas-item "B2AAEE56-42D8-42C3-8F67-865473F50E8D")
                                 (yas-separator)
                                 ;; XHTML — 1.0 Frameset
                                 (yas-item "9ED6ABBE-A802-11D9-BFC8-000D93C8BE28")
                                 ;; XHTML — 1.0 Strict
                                 (yas-item "C8B83564-A802-11D9-BFC8-000D93C8BE28")
                                 ;; XHTML — 1.0 Transitional
                                 (yas-item "7D8C2F74-A802-11D9-BFC8-000D93C8BE28")
                                 ;; XHTML — 1.1
                                 (yas-item "5CE8FC6E-A802-11D9-BFC8-000D93C8BE28")))
                   (yas-submenu "Insert Tag"
                                (;; Ignoring CodeCompletion HTML Tags
                                 (yas-ignore-item "3463E85F-F500-49A0-8631-D78ED85F9D60")
                                 
                                 ;; Base
                                 (yas-item "4462A6B8-A08A-11D9-A5A2-000D93C8BE28")
                                 ;; Body
                                 (yas-item "4905D47B-A08B-11D9-A5A2-000D93C8BE28")
                                 ;; Br
                                 (yas-item "3E008E42-A5C9-11D9-9BCD-000D93C8BE28")
                                 ;; Div
                                 (yas-item "576036C0-A60E-11D9-ABD6-000D93C8BE28")
                                 ;; Embed QT Movie
                                 (yas-item "42F15753-9B6D-4DD8-984C-807B94363277")
                                 ;; Fieldset
                                 (yas-item "9BD2BE01-A854-4D55-B584-725D04C075C0")
                                 ;; Form
                                 (yas-item "232C2E8B-A08E-11D9-A5A2-000D93C8BE28")
                                 ;; Head
                                 (yas-item "9CF008C4-A086-11D9-A5A2-000D93C8BE28")
                                 ;; Heading
                                 (yas-item "65BA66DC-A07F-11D9-A5A2-000D93C8BE28")
                                 ;; Input
                                 (yas-item "44180979-A08E-11D9-A5A2-000D93C8BE28")
                                 ;; Input with Label
                                 (yas-item "D8DCCC81-749A-4E2A-B4BC-D109D5799CAA")
                                 ;; Link
                                 (yas-item "77BFD0C0-A08A-11D9-A5A2-000D93C8BE28")
                                 ;; Mail Anchor
                                 (yas-item "81DA4C74-A530-11D9-9BCD-000D93C8BE28")
                                 ;; Meta
                                 (yas-item "DA99AC44-A083-11D9-A5A2-000D93C8BE28")
                                 ;; Option
                                 (yas-item "5820372E-A093-4F38-B25C-B0CCC50A0FC4")
                                 ;; Script
                                 (yas-item "6592050A-A087-11D9-A5A2-000D93C8BE28")
                                 ;; Script With External Source
                                 (yas-item "7D676C4C-A087-11D9-A5A2-000D93C8BE28")
                                 ;; Select Box
                                 (yas-item "26023CFF-C73F-4EF5-9803-E4DBA2CBEADD")
                                 ;; Style
                                 (yas-item "3C518074-A088-11D9-A5A2-000D93C8BE28")
                                 ;; Table
                                 (yas-item "57176082-A12F-11D9-A5A2-000D93C8BE28")
                                 ;; Text Area
                                 (yas-item "AAC9D7B8-A12C-11D9-A5A2-000D93C8BE28")
                                 ;; Title
                                 (yas-item "B62ECABE-A086-11D9-A5A2-000D93C8BE28")))
                   
                   (yas-submenu "Includes"
                                (;; Ignoring Add Persistent Include
                                 (yas-ignore-item "0D814247-7A00-46EE-A2A4-45FBBF4B1181")
                                 ;; Ignoring Update Document
                                 (yas-ignore-item "4400BCE9-20E3-426E-B1D7-2C0BCA53BCF8")
                                 ;; Ignoring Update Project / Selected Files
                                 (yas-ignore-item "CA24BD98-E4B6-48F8-B15A-84CC533BE1BD")
                                 ;; Ignoring Help: Persistent Includes
                                 (yas-ignore-item "9AFDEB2C-D9F0-423E-8211-EBB089F51F0C")))
                   (yas-submenu "Format"
                                (;; Strong
                                 (yas-item "4117D930-B6FA-4022-97E7-ECCAF4E70F63")
                                 ;; Emphasize
                                 (yas-item "EBB98620-3292-4621-BA38-D8A9A65D9551")))
                   (yas-submenu "Conditional Comments"
                                (;; IE Conditional Comment: Internet Explorer
                                 (yas-item "0ED6DA73-F38F-4A65-B18F-3379D2BA9387")
                                 ;; IE Conditional Comment: Internet Explorer 5.0 only
                                 (yas-item "3A517A94-001E-464D-8184-1FE56D0D0D70")
                                 ;; IE Conditional Comment: Internet Explorer 5.5 only
                                 (yas-item "E3F8984E-7269-4981-9D30-967AB56A6ACE")
                                 ;; IE Conditional Comment: Internet Explorer 5.x
                                 (yas-item "F3512848-7889-45DA-993B-0547976C8E6D")
                                 ;; IE Conditional Comment: Internet Explorer 6 and below
                                 (yas-item "32BBB9AB-8732-4F91-A587-354941A27B69")
                                 ;; IE Conditional Comment: Internet Explorer 6 only
                                 (yas-item "48DF7485-52EA-49B3-88AF-3A41F933F325")
                                 ;; IE Conditional Comment: Internet Explorer 7 and above
                                 (yas-item "CBC24AF4-88E0-498B-BE50-934B9CF29EC7")
                                 ;; IE Conditional Comment: NOT Internet Explorer
                                 (yas-item "F00170EE-4A82-413F-A88B-85293E69A88B")))
                   
                   ;; Wrap Selection in Open/Close Tag
                   (yas-item "BC8B8AE2-5F16-11D9-B9C3-000D93589AF6")
                   ;; Wrap Each Selected Line in Open/Close Tag
                   (yas-item "991E7EBD-F3F5-469A-BA01-DC30E04AD472")
                   ;; Wrap in <?= … ?>
                   (yas-item "912906A0-9A29-434B-AE98-E9DFDE6E48B4")
                   (yas-separator)
                   ;; Ignoring Strip HTML Tags from Document / Selection
                   (yas-ignore-item "20D760B5-A127-11D9-A5A2-000D93C8BE28")
                   ;; Ignoring Tidy
                   (yas-ignore-item "45F92B81-6F0E-11D9-A1E4-000D9332809C"))
                    '("7B7E945E-A112-11D9-A5A2-000D93C8BE28"
                       "3C44EABE-8D6F-4B1B-AB91-F419FAD1A0AD"
                       "9AFDEB2C-D9F0-423E-8211-EBB089F51F0C"
                       "CBD82CF3-74E9-4E7A-B3F6-9348754EB5AA"
                       "3463E85F-F500-49A0-8631-D78ED85F9D60"
                       "9B13543F-8356-443C-B6E7-D9259B604927"
                       "0D814247-7A00-46EE-A2A4-45FBBF4B1181"
                       "AC5F664E-86BA-4D81-B6CE-1B12F69FA490"
                       "4400BCE9-20E3-426E-B1D7-2C0BCA53BCF8"
                       "CA24BD98-E4B6-48F8-B15A-84CC533BE1BD"
                       "6B024865-6095-4CE3-8EDD-DC6F2230C2FF"
                       "3DD8406C-A116-11D9-A5A2-000D93C8BE28"
                       "43C9E8AE-3E53-4B82-A1AF-56697BB3EF09"
                       "C183920D-A126-11D9-A5A2-000D93C8BE28"
                       "2C4C9673-B166-432A-8938-75A5CA622481"
                       "0658019F-3635-462E-AAC2-74E4FE508A9B"
                       "89E5CC0A-3EFF-4DEF-A299-2E9651DE6529"
                       "B8651C6E-A05E-11D9-86AC-000D93C8BE28"
                       "20D760B5-A127-11D9-A5A2-000D93C8BE28"
                       "45F92B81-6F0E-11D9-A1E4-000D9332809C"
                       "3F26240E-6E4A-11D9-B411-000D93589AF6"
                       "B23D6E15-6B33-11D9-86C1-000D93589AF6"
                       "C8B717C2-6B33-11D9-BB47-000D93589AF6"
                       "CD6D2CC6-6B33-11D9-BDFD-000D93589AF6"
                       "7B7E945E-A112-11D9-A5A2-000D93C8BE28"
                       "04332FA8-8157-46C4-9854-8C190FFD96C6"
                       "E6F19171-F664-4B4F-92DA-3E15E6CAD35C"
                       "26068A55-4C84-409D-BA00-162B55AF6961"
                       "EBEE6B51-29C7-4362-818F-A190CACD5296"
                       "65D38039-6B0A-48E9-9E49-43832ECC4107"
                       "CDE8EFD6-9DE2-4E8C-BB6A-52E8CCD2E977"))

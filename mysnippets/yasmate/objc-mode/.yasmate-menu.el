;; Translated menu
;; 
(yas-define-menu 'objc-mode
                 '(;; Ignoring Help
                   (yas-ignore-item "AFB40870-6F83-4211-9362-0538287B52A9")
                   ;; Ignoring Documentation for Word / Selection
                   (yas-ignore-item "2E0F350A-7B23-11D9-B084-000D93589AF6")
                   
                   (yas-submenu "Language Boilerplate"
                                (;; #import <…>
                                 (yas-item "20241464-7299-11D9-813A-000D93589AF6")
                                 ;; #import "…"
                                 (yas-item "1E3A92DA-7299-11D9-813A-000D93589AF6")
                                 (yas-separator)
                                 ;; Class
                                 (yas-item "BC8B9C24-5F16-11D9-B9C3-000D93589AF6")
                                 ;; Class Implementation
                                 (yas-item "BE0B2832-D88E-40BF-93EE-281DDA93840B")
                                 ;; Class Interface
                                 (yas-item "06F15373-9900-11D9-9BB8-000A95A89C98")
                                 ;; Category
                                 (yas-item "27AC6270-9900-11D9-9BB8-000A95A89C98")
                                 ;; Category Implementation
                                 (yas-item "3E270C37-E7E2-4D1D-B28F-CEDD8DE0041C")
                                 ;; Category Interface
                                 (yas-item "596B13EC-9900-11D9-9BB8-000A95A89C98")
                                 (yas-separator)
                                 ;; Method
                                 (yas-item "BC8B9DD7-5F16-11D9-B9C3-000D93589AF6")
                                 ;; Method: Initialize
                                 (yas-item "366DBAB0-554B-4A38-966E-793DFE13A1EC")
                                 ;; Class Method
                                 (yas-item "1251B9E2-6BF0-11D9-8384-000D93589AF6")
                                 ;; Sub-method (Call Super)
                                 (yas-item "BC8B9E72-5F16-11D9-B9C3-000D93589AF6")
                                 (yas-separator)
                                 ;; IBOutlet
                                 (yas-item "30C260A7-AFB1-11D9-9D48-000D93589AF6")))
                   (yas-submenu "Accessor Methods For"
                                (;; Property (Objective-C 2.0)
                                 (yas-item "EE603767-8BA3-4F54-8DE5-0C9E64BE5DF7")
                                 ;; Synthesize Property
                                 (yas-item "C0B942C9-07CE-46B6-8FAE-CB8496F9F544")
                                 (yas-separator)
                                 ;; CoreData
                                 (yas-item "563B2FDB-A163-46FE-9380-4178EFC1AD14")
                                 ;; String
                                 (yas-item "5449EC50-98FE-11D9-9BB8-000A95A89C98")
                                 ;; Object
                                 (yas-item "65844040-1D13-4F29-98CC-E742F151527F")
                                 ;; Primitive Type
                                 (yas-item "DADC6C91-415F-463A-9C24-7A059BB5EE56")
                                 (yas-separator)
                                 ;; KVC Array
                                 (yas-item "DECC6BAC-94AF-429A-8609-D101C940D18D")))
                   (yas-submenu "Common Method Calls"
                                (;; Bind Property to Key Path of Object
                                 (yas-item "59FC2842-A645-11D9-B2CB-000D93589AF6")
                                 ;; Register for Notification
                                 (yas-item "E8107901-70F1-45D9-8633-81BD5E57CC89")
                                 ;; Detach New NSThread
                                 (yas-item "25AD69B4-905B-4EBC-A3B3-0BAB6D8BDD75")
                                 (yas-separator)
                                 ;; Read Defaults Value
                                 (yas-item "3EF96A1F-B597-11D9-A114-000D93589AF6")
                                 ;; Write Defaults Value
                                 (yas-item "53672612-B597-11D9-A114-000D93589AF6")
                                 (yas-separator)
                                 ;; NSLog(…)
                                 (yas-item "1251B7E8-6BF0-11D9-8384-000D93589AF6")
                                 ;; NSRunAlertPanel
                                 (yas-item "9EF84198-BDAF-11D9-9140-000D93589AF6")))
                   (yas-submenu "Object Instantiations"
                                (;; NSArray
                                 (yas-item "BC8B9CAD-5F16-11D9-B9C3-000D93589AF6")
                                 ;; NSDictionary
                                 (yas-item "BC8B9D3A-5F16-11D9-B9C3-000D93589AF6")
                                 ;; NSBezierPath
                                 (yas-item "917BA9ED-9A62-11D9-9A65-000A95A89C98")
                                 ;; NSString With Format
                                 (yas-item "B07879C7-F1E0-4606-93F1-1A948965CD0E")))
                   (yas-submenu "Idioms"
                                (;; Lock Focus
                                 (yas-item "3F57DB1B-9373-46A6-9B6E-19F2D25658DE")
                                 ;; Save and Restore Graphics Context
                                 (yas-item "F2D5B215-2C10-40BC-B973-0A859A3E3CBD")
                                 ;; Autorelease Pool
                                 (yas-item "D402B10A-149B-414D-9961-110880389A8E")
                                 (yas-separator)
                                 ;; Responds to Selector
                                 (yas-item "171FBCAE-0D6F-4D42-B24F-871E3BB6DFF0")
                                 ;; Delegate Responds to Selector
                                 (yas-item "622842E6-11F7-4D7B-A322-F1B8A1FE8CE5")))
                   
                   ;; Ignoring Insert [[[… alloc] init] autorelease]
                   (yas-ignore-item "EA820F17-FD1D-4E7A-9961-E75F7D360968")
                   ;; Ignoring Insert Call to Super
                   (yas-ignore-item "DA9B35AF-938D-4166-8576-E8E3C73F0739")
                   ;; Ignoring Insert Matching Start Bracket
                   (yas-ignore-item "DB16585F-4D78-412B-B468-38AD54C254B5")
                   ;; Ignoring Paste Implementation / Interface
                   (yas-ignore-item "CB5EC7EC-35B7-4FD8-9045-31CCC379D474")
                   ;; Ignoring Paste selector
                   (yas-ignore-item "D9CA98D1-7564-4CCB-8156-9A06210A1A7F")
                   ;; Ignoring Delete Outer Method Call
                   (yas-ignore-item "E802FA1A-1E2E-4F8A-957F-C1533CE57400")
                   
                   ;; @selector(…)
                   (yas-item "7829F2EC-B8BA-11D9-AE51-000393A143CC")
                   (yas-separator)
                   ;; Ignoring Index Headers for Completion
                   (yas-ignore-item "42B1691B-DC28-4743-9B18-C8D51B70722C")
                   ;; Ignoring Insert NSLog() for Current Method
                   (yas-ignore-item "C5624A26-E661-46EE-AA6A-14E6B678CFF9")
                   ;; Ignoring Completion: Inside @selector
                   (yas-ignore-item "F929835A-C9F7-4934-87BD-05FD11C4435B"))
                    '("A3555C49-D367-4CF5-8032-13B291820CD3"
                       "478FBA1D-C11C-4D53-BE95-8B8ABB5F15DC"
                       "88754B0F-D8DB-4796-9D02-058B756C606D"
                       "30E93FBA-5A81-4D94-8A03-9CD46FCA3CFA"
                       "8AF46225-833C-473E-8EEC-F21C581636F6"
                       "8957C99F-88F5-42CC-B355-AAC6BF3FDF8D"
                       "122E10C1-DFA2-4A9E-9B17-8EBFA6E10CC7"
                       "35EB2F86-DEA0-443B-8DC2-4815F0478F67"
                       "013BFEBB-A744-46F1-94A5-F851635E00FA"
                       "BA432891-294B-47A4-BECF-F3C95B3766C1"
                       "C125E6DB-7FB5-4B19-8648-0A5617B1B5BC"
                       "325B0A2B-5939-4805-80A1-6DED5B373283"
                       "9D01148D-1073-40D2-936E-FFF67580D2B3"
                       "478FBA1D-C11C-4D53-BE95-8B8ABB5F15DC"
                       "88754B0F-D8DB-4796-9D02-058B756C606D"
                       "30E93FBA-5A81-4D94-8A03-9CD46FCA3CFA"
                       "F929835A-C9F7-4934-87BD-05FD11C4435B"
                       "E802FA1A-1E2E-4F8A-957F-C1533CE57400"
                       "8AF46225-833C-473E-8EEC-F21C581636F6"
                       "42B1691B-DC28-4743-9B18-C8D51B70722C"
                       "AFB40870-6F83-4211-9362-0538287B52A9"
                       "DB16585F-4D78-412B-B468-38AD54C254B5"
                       "C5624A26-E661-46EE-AA6A-14E6B678CFF9"
                       "CB5EC7EC-35B7-4FD8-9045-31CCC379D474"
                       "D9CA98D1-7564-4CCB-8156-9A06210A1A7F"
                       "8957C99F-88F5-42CC-B355-AAC6BF3FDF8D"
                       "A8F23393-4D73-480A-A268-6DCD514DE2E4"
                       "122E10C1-DFA2-4A9E-9B17-8EBFA6E10CC7"
                       "DA9B35AF-938D-4166-8576-E8E3C73F0739"
                       "2E0F350A-7B23-11D9-B084-000D93589AF6"
                       "EA820F17-FD1D-4E7A-9961-E75F7D360968"
                       "BB1916F0-C021-11D9-93C5-000D93589AF6"))

-- This example implements a dynamic web page to edit persons
-- consisting of name, email, and date of birth.

module Persons where

import Global
import Time ( validDate )

import HTML.Base
import HTML.Session
import HTML.WUI

-- An action that just shows its argument in HTML format.
showResult :: Show a => a -> IO [BaseHtml]
showResult v = return [htxt ("Modified value: "++show v)]

-- A date WUI.
wDate :: WuiSpec (Int,Int,Int)
wDate = wTriple (wSelectInt [1..31]) (wSelectInt [1..12]) wInt
           `withCondition` correctDate
           `withError` "Illegal date:"

--- A condition for correct dates.
correctDate :: (Int,Int,Int) -> Bool
correctDate (d,m,y) = validDate y m d

-- An email WUI.
wEmail :: WuiSpec String
wEmail = wStringSize 20 `withCondition` correctEmail
                        `withError` "Invalid email address:"

--- A simple condition for syntactically correct email addresses.
correctEmail :: String -> Bool
correctEmail s = not (null (snd (break ('@'==) s)))

-- The type of persons in the WUI:
type Person = (String,String,String,(Int,Int,Int))

-- A person WUI.
wPerson :: WuiSpec Person
wPerson =
  w4Tuple (wRequiredStringSize 12) (wRequiredStringSize 12) wEmail wDate

--- The data stored for executing the WUI form.
wuiPersonStore :: Global (SessionStore (WuiStore [Person]))
wuiPersonStore = global emptySessionStore (Persistent "wuiPersonStore")

--- The WUI form definition for persons.
--- The store operation simply shows the result.
wuiForm :: HtmlFormDef (WuiStore [Person])
wuiForm = wui2FormDef "Persons.wuiForm" wuiPersonStore (wList wPerson)
                      showResult wuiPersonFormat
 where
  wuiPersonFormat inputhexp storehandler =
    [inputhexp, breakline,
     button "Submit"
       (\env -> storehandler env >>= return . headerPage "Person WUI Test")]


-- The main HTML page containing the form.
main :: IO HtmlPage
main = do
  cookie <- sessionCookie  -- be sure that there is a cookie for the session
  setWuiStore wuiPersonStore persons -- initialize WUI store
  return (headerPage "Person WUI Test"
            [ formElem wuiForm ] `addPageParam` cookie)

-- Some example data.
persons :: [Person]
persons = [("Bob","Carter","bob@carter.com",(3,10,1965))
          ,("Bill","Jones","billy@acm.org",(29,2,1982))
          ,("Joe","Smith","smith.org",(20,3,1978))
          ]

-- Install the CGI script in user homepage by:
-- > cypm exec curry2cgi -o ~/public_html/cgi-bin/persons.cgi Persons

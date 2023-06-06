------------------------------------------------------------------------------
--- A library to support the type-oriented construction of Web User Interfaces
--- (WUIs).
---
--- The ideas behind the application and implementation of WUIs are
--- described in a paper that is available via
--- [this web page](http://www.informatik.uni-kiel.de/~pakcs/WUI).
---
--- @author Michael Hanus
--- @version June 2023
------------------------------------------------------------------------------

{-# OPTIONS_FRONTEND -Wno-incomplete-patterns #-}

module HTML.WUI
  ( --WuiState,cgiRef2state,state2cgiRef,value2state,state2value,
    --states2state,state2states,altstate2state,state2altstate,
    Rendering,WuiSpec,
    withRendering,withError,withCondition,adaptWSpec,transformWSpec,
    wHidden,wConstant,wInt,
    wString,wStringSize,wRequiredString,wRequiredStringSize,wTextArea,
    wPassword, wPasswordSize,
    wSelect,wSelectInt,wSelectBool,wRadioSelect,wRadioBool,wCheckBool,
    wMultiCheckSelect,
    wPair,wTriple,w4Tuple,w5Tuple,w6Tuple,w7Tuple,w8Tuple,
    w9Tuple,w10Tuple,w11Tuple,w12Tuple,w13Tuple,w14Tuple,
  
    -- these parameterized constructor combinators cause
    -- non-determinism in KiCS2:
    wCons2,wCons3,wCons4,wCons5,wCons6,wCons7,wCons8,
    wCons9,wCons10,wCons11,wCons12,wCons13,wCons14,
  
    wJoinTuple,wMaybe,wCheckMaybe,wRadioMaybe,
    wList,wListWithHeadings,wHList,wMatrix,wEither,
    WTree(..),wTree,
    WuiHandler,wuiHandler2button,
    renderTuple,renderTaggedTuple,renderList,
    WuiStore, WuiSessionStore, setWuiStore, wui2FormDef,
    ParWuiSessionStore, setParWuiStore, pwui2FormDef,
    wuiSimpleRenderer
  )
 where

import Data.Char               ( isDigit, isSpace )
import Data.List               ( elemIndex )

import Data.Function.Inversion ( invf1 )
import HTML.Base
import HTML.Session

infixl 0 `withRendering`
infixl 0 `withError`
infixl 0 `withCondition`

------------------------------------------------------------------------------
--- An internal WUI state is used to maintain the cgi references of the input
--- fields as a structure that corresponds to the structure of the edit data.
data WuiState =
     Ref HtmlRef            -- reference to elementary input field
   | Hidden String          -- string representation of a hidden value
   | CompNode [WuiState]    -- composition of trees (substructures)
   | AltNode (Int,WuiState) -- alternative of trees (union of substructures)

cgiRef2state :: HtmlRef -> WuiState
cgiRef2state cr = Ref cr

state2cgiRef :: WuiState -> HtmlRef
state2cgiRef (Ref cr) = cr

value2state :: Show a => a -> WuiState
value2state v = Hidden (show v)

state2value :: Read a => WuiState -> a
state2value (Hidden s) = read s

states2state :: [WuiState] -> WuiState
states2state sts = CompNode sts

state2states :: WuiState -> [WuiState]
state2states (CompNode sts) = sts

altstate2state :: (Int,WuiState) -> WuiState
altstate2state alt = AltNode alt

state2altstate :: WuiState -> (Int,WuiState)
state2altstate (AltNode alt) = alt

------------------------------------------------------------------------------
--- A rendering is a function that combines the visualization of components
--- of a data structure into some HTML expression.
type Rendering = [HtmlExp] -> HtmlExp

--- WuiParams specify the parameters of an individual Wui component type:
--- * the standard rendering
--- * an error message shown in case of illegal inputs
--- * a condition to specify legal input values
type WuiParams a = (Rendering, String, a -> Bool)

renderOf :: WuiParams a -> Rendering
renderOf (render,_,_) = render

errorOf :: WuiParams a -> String
errorOf (_,err,_) = err

conditionOf :: WuiParams a -> (a -> Bool)
conditionOf (_,_,c) = c

------------------------------------------------------------------------------
--- The type HtmlSate are values consisting of an HTML expression
--- (usually containing some input elements) and a WUI state containing
--- references to input elements in the HTML expression.

type HtmlState = (HtmlExp,WuiState)

------------------------------------------------------------------------------
--- A handler for a WUI is an event handler for HTML forms possibly with some
--- specific code attached (for future extensions).
data WuiHandler = WHandler HtmlHandler

--- Transform a WUI handler into a submit button with a given label string.
wuiHandler2button :: String -> WuiHandler -> HtmlExp
wuiHandler2button title (WHandler handler) = button title handler

------------------------------------------------------------------------------
--- The type of WUI specifications.
--- The first component are parameters specifying the behavior of this WUI type
--- (rendering, error message, and constraints on inputs).
--- The second component is a "show" function returning an HTML expression for
--- the edit fields and a WUI state containing the HtmlRefs to extract
--- the values from the edit fields. If the second component of the show
--- function is `True`, then the WUI condition is not checked for the data,
--- otherwise the data is rendered with an error message if the
--- WUI condition does not hold for the data.
--- The third component is a predicate to check the correctness of
--- the current data (with all its subcomponents).
--- The fourth component is "read" function to extract the values from
--- the edit fields for a given cgi environment.
data WuiSpec a =
  WuiSpec (WuiParams a)
          (WuiParams a -> Bool -> a -> HtmlState)
          (WuiParams a -> a -> Bool)
          (HtmlEnv -> WuiState -> a)

--- Puts a new rendering function into a WUI specification.
withRendering :: WuiSpec a -> Rendering -> WuiSpec a
withRendering (WuiSpec (_,errmsg,legal) showhtml correct readvalue) render =
  WuiSpec (render,errmsg,legal) showhtml correct readvalue


--- Puts a new error message into a WUI specification.
withError :: WuiSpec a -> String -> WuiSpec a
withError (WuiSpec (render,_,legal) showhtml correct readvalue) errmsg =
  WuiSpec (render,errmsg,legal) showhtml correct readvalue

--- Puts a new condition into a WUI specification.
withCondition :: WuiSpec a -> (a -> Bool) -> WuiSpec a
withCondition (WuiSpec (render,errmsg,_) showhtml correct readvalue) legal =
              (WuiSpec (render,errmsg,legal) showhtml correct readvalue)

--- Transforms a WUI specification from one type to another.
transformWSpec :: (a->b,b->a) -> WuiSpec a -> WuiSpec b
transformWSpec (a2b,b2a) (WuiSpec wparamsa showhtmla correcta readvaluea) =
  WuiSpec (transParam b2a wparamsa)
          (\wparamsb nchk b -> showhtmla (transParam a2b wparamsb) nchk (b2a b))
          (\_ b -> correcta wparamsa (b2a b))
          (\env wst -> a2b (readvaluea env wst))
 where
  transParam :: (b->a) -> WuiParams a -> WuiParams b
  transParam toa (render,errmsg,legal) = (render,errmsg,legal . toa)

--- Adapt a WUI specification to a new type. For this purpose,
--- the first argument must be a transformation mapping values
--- from the old type to the new type. This function must be bijective
--- and operationally invertible (i.e., the inverse must be computable
--- by narrowing). Otherwise, use <code>transformWSpec</code>!
adaptWSpec :: (Data a, Data b) => (a -> b) -> WuiSpec a -> WuiSpec b
adaptWSpec a2b = transformWSpec (a2b, invf1 a2b)

------------------------------------------------------------------------------
-- A collection of basic WUIs and WUI combinators:

--- A hidden widget for a value that is not shown in the WUI.
--- Usually, this is used in components of larger
--- structures, e.g., internal identifiers, data base keys.
wHidden :: (Read a, Show a) => WuiSpec a
wHidden =
  WuiSpec (head, "?", const True) -- dummy values, not used
          (\_ _ v -> (hempty, value2state v))
          (\_ _ -> True)
          (\_ wst -> (state2value wst))

--- A widget for values that are shown but cannot be modified.
--- The first argument is a mapping of the value into a HTML expression
--- to show this value.
wConstant :: (Read a, Show a) => (a -> HtmlExp) -> WuiSpec a
wConstant showhtml =
  WuiSpec (head, "?", const True)
          (\wparams _ v -> ((renderOf wparams) [showhtml v], value2state v))
          (\_ _ -> True)
          (\_ wst -> state2value wst)

------------------------------------------------------------------------------
--- A widget for editing integer values.
wInt :: WuiSpec Int
wInt =
  WuiSpec (head,"Illegal integer:",const True)
          (checkLegalInput intWidget)
          (\wparams -> conditionOf wparams)
          (\env wst ->
            let input = env (state2cgiRef wst)
             in maybe 0 id (readMaybeInt (stripSpaces input)))
 where
  intWidget render i = let ref free in
    (render [textField ref (show i) `addAttr` ("size","6")], cgiRef2state ref)

-- Remove leading and ending spaces in a string.
stripSpaces :: String -> String
stripSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Read a (possibly negative) integer in a string.
-- Return Nothing is this is not an integer string.
readMaybeInt :: String -> Maybe Int
readMaybeInt "" = Nothing
readMaybeInt (v:s) | v=='-'  = maybe Nothing (\i->Just (-i)) (acc 0 s)
                   | isDigit v  = acc 0 (v:s)
                   | otherwise  = Nothing
 where
  acc n "" = Just n
  acc n (c:cs) | isDigit c = acc (10*n + ord c - ord '0') cs
               | otherwise = Nothing

checkLegalInput :: (Rendering -> a -> HtmlState) -> WuiParams a -> Bool -> a
                -> HtmlState
checkLegalInput value2widget (render,errmsg,legal) nocheck value =
  if nocheck || legal value
    then value2widget render value
    else value2widget (renderError render errmsg) value

------------------------------------------------------------------------------
-- WUIs for strings.

--- A predefined filter for processing string inputs.
--- Here, we replace \r\n by \n:
filterStringInput :: String -> String
filterStringInput = removeCRs

--- Replace all \r\n by \n:
removeCRs :: String -> String
removeCRs [] = []
removeCRs [c] = [c]
removeCRs (c1:c2:cs) =
  if c1=='\r' && c2=='\n' then '\n' : removeCRs cs
                          else c1 : removeCRs (c2:cs)

--- A widget for editing string values.
wString :: WuiSpec String
wString = wStringAttrs []

--- A widget for editing string values with a size attribute.
wStringSize :: Int -> WuiSpec String
wStringSize size = wStringAttrs [("size",show size)]

--- A widget for editing string values with some attributes for the
--- text field.
wStringAttrs :: [(String,String)] -> WuiSpec String
wStringAttrs attrs =
  WuiSpec (head, "?", const True)
          (checkLegalInput stringWidget)
          (\wparams -> conditionOf wparams)
          (\env s -> filterStringInput (env (state2cgiRef s)))
 where
  stringWidget render v = let ref free in
    (render [foldr (flip addAttr) (textField ref v) attrs], cgiRef2state ref)

--- A widget for editing string values that are required to be non-empty.
wRequiredString :: WuiSpec String
wRequiredString =
  wString `withError`     "Missing input:"
          `withCondition` (not . null)

--- A widget with a size attribute for editing string values
--- that are required to be non-empty.
wRequiredStringSize :: Int -> WuiSpec String
wRequiredStringSize size =
  wStringSize size `withError`     "Missing input:"
                   `withCondition` (not . null)

--- A widget for editing string values in a text area.
--- The argument specifies the height and width of the text area.
wTextArea :: (Int,Int) -> WuiSpec String
wTextArea dims =
  WuiSpec (head, "?", const True)
          (checkLegalInput textareaWidget)
          (\wparams -> conditionOf wparams)
          (\env s -> filterStringInput (env (state2cgiRef s)))
 where
  textareaWidget render v = let ref free in
    (render [textArea ref dims v], cgiRef2state ref)

------------------------------------------------------------------------------
-- WUIs for password which are not visible

--- A widget for entering a password.
--- The contents is not visible and, by default,
--- values are required to be non-empty.
wPassword :: WuiSpec String
wPassword = wPasswordAttrs []

--- A widget with a size attribute for entering a password.
--- The contents is not visible and, by default,
--- values are required to be non-empty.
wPasswordSize :: Int -> WuiSpec String
wPasswordSize size = wPasswordAttrs [("size",show size)]

--- A widget for editing string values with some attributes for the
--- password field.
wPasswordAttrs :: [(String,String)] -> WuiSpec String
wPasswordAttrs attrs =
  WuiSpec (head, "Missing input:", (not . null))
          (checkLegalInput passwordWidget)
          (\wparams -> conditionOf wparams)
          (\env s -> env (state2cgiRef s))
 where
  passwordWidget render _ = let ref free in
    (render [foldr (flip addAttr) (password ref) attrs], cgiRef2state ref)

------------------------------------------------------------------------------
--- A widget to select a value from a given list of values.
--- The current value should be contained in the value list and is preselected.
--- The first argument is a mapping from values into strings to be shown
--- in the selection widget.
wSelect :: Eq a => (a -> String) -> [a] -> WuiSpec a
wSelect showelem selset =
  WuiSpec (head, "?", const True)
          (checkLegalInput selWidget)
          (\wparams -> conditionOf wparams)
          (\env s -> selset !! read (env (state2cgiRef s)))
 where
  selWidget render v =
    let ref free
        idx = elemIndex v selset
        namevalues = zip (map showelem selset) (map show [0..])
     in (render [maybe (selection ref namevalues)
                       (\i -> selectionInitial ref namevalues i)
                       idx],
         cgiRef2state ref)

--- A widget to select a value from a given list of integers (provided as
--- the argument).
--- The current value should be contained in the value list and is preselected.
wSelectInt :: [Int] -> WuiSpec Int
wSelectInt = wSelect show

--- A widget to select a Boolean value via a selection box.
--- The arguments are the strings that are shown for the values
--- True and False in the selection box, respectively.
--- @param true - string for selection of True
--- @param false - string for selection of False
--- @return a WUI specification for a Boolean selection widget
wSelectBool :: String -> String -> WuiSpec Bool
wSelectBool true false = wSelect (\b -> if b then true else false) [True,False]

--- A widget to select a Boolean value via a check box.
--- The first argument are HTML expressions that are shown after the
--- check box.  The result is True if the box is checked.
wCheckBool :: [HtmlExp] -> WuiSpec Bool
wCheckBool hexps =
  WuiSpec (head, "?", const True)
          (checkLegalInput checkWidget)
          (\wparams -> conditionOf wparams)
          (\env wst -> env (state2cgiRef wst)=="True")
 where
  checkWidget render v = let ref free in
    (render [inline ((if v then checkedBox else checkBox) ref "True" : hexps)],
     cgiRef2state ref)

--- A widget to select a list of values from a given list of values
--- via check boxes.
--- The current values should be contained in the value list and are preselected.
--- The first argument is a mapping from values into HTML expressions
--- that are shown for each item after the check box.
wMultiCheckSelect :: Eq a => (a -> [HtmlExp]) -> [a] -> WuiSpec [a]
wMultiCheckSelect showelem selset =
  WuiSpec (renderTuple, tupleError, const True)
          (checkLegalInput checkWidget)
          (\wparams -> conditionOf wparams)
          (\env st ->
              concatMap (\ (ref,s) -> if env ref=="True" then [s] else [])
                        (zip (map state2cgiRef (state2states st)) selset))
 where
  checkWidget render vs =
    let refs = take (length selset) newVars
        numsetitems = zip refs selset
        showItem (ref,s) =
           inline ((if s `elem` vs then checkedBox else checkBox)
                                                       ref "True" : showelem s)
     in (render (map showItem numsetitems),
         states2state (map cgiRef2state refs))

newVars :: Data a => [a]
newVars = unknown : newVars

--- A widget to select a value from a given list of values via a radio button.
--- The current value should be contained in the value list and is preselected.
--- The first argument is a mapping from values into HTML expressions
--- that are shown for each item after the radio button.
wRadioSelect :: Eq a => (a->[HtmlExp]) -> [a] -> WuiSpec a
wRadioSelect showelem selset =
  WuiSpec (renderTuple, tupleError, const True)
          (checkLegalInput radioWidget)
          (\wparams -> conditionOf wparams)
          (\env s -> selset !! read (env (state2cgiRef s)))
 where
  radioWidget render v =
    let ref free
        idx = maybe 0 id (elemIndex v selset)
        numhitems = zip [0..] (map showelem selset)
        showItem (i,s) = table [[[(if i==idx then radioMain else radioOther)
                                        ref (show i)],s]]
     in (render (map showItem numhitems),
         cgiRef2state ref)

--- A widget to select a Boolean value via a radio button.
--- The arguments are the lists of HTML expressions that are shown after
--- the True and False radio buttons, respectively.
--- @param true - HTML expressions for True radio button
--- @param false - HTML expressions for False radio button
--- @return a WUI specification for a Boolean selection widget
wRadioBool :: [HtmlExp] -> [HtmlExp] -> WuiSpec Bool
wRadioBool truehexps falsehexps =
  wRadioSelect (\b -> if b then truehexps else falsehexps) [True,False]

--- WUI combinator for pairs.
wPair :: (Data a, Data b) => WuiSpec a -> WuiSpec b -> WuiSpec (a,b)
-- This simple implementation does not work in KiCS2 due to non-determinism
-- cause by functional patterns:
-- wPair = wCons2 (\a b -> (a,b))
wPair (WuiSpec wparamsa showa cora reada) (WuiSpec wparamsb showb corb readb) =
  WuiSpec (renderTuple, tupleError, const True) showc corc readc
 where
  showc (render,errmsg,legal) nocheck (va,vb) =
    let (hea,rta) = showa wparamsa nocheck va
        (heb,rtb) = showb wparamsb nocheck vb
     in ((if nocheck || legal (va,vb)
            then render
            else renderError render errmsg) [hea,heb], states2state [rta,rtb])

  corc wparamsc (va,vb) = conditionOf wparamsc (va,vb) &&
                          cora wparamsa va && corb wparamsb vb

  readc env s =
    let [ra,rb] = state2states s
    in (reada env ra, readb env rb)

--- WUI combinator for constructors of arity 2.
--- The first argument is the binary constructor.
--- The second and third arguments are the WUI specifications
--- for the argument types.
wCons2 :: (Data a, Data b, Data c) =>
          (a->b->c) -> WuiSpec a -> WuiSpec b -> WuiSpec c
wCons2 cons (WuiSpec wparamsa showa cora reada)
            (WuiSpec wparamsb showb corb readb) =
  WuiSpec (renderTuple, tupleError, const True) showc corc readc
 where
  showc (render,errmsg,legal) nocheck vc | cons va vb =:<= vc =
    let (hea,rta) = showa wparamsa nocheck va
        (heb,rtb) = showb wparamsb nocheck vb
     in ((if nocheck || legal (cons va vb)
            then render
            else renderError render errmsg) [hea,heb], states2state [rta,rtb])
   where va,vb free

  corc wparamsc vc | cons va vb =:<= vc =
    conditionOf wparamsc (cons va vb) &&
    cora wparamsa va && corb wparamsb vb            where va,vb free

  readc env s =
    let [ra,rb] = state2states s
    in cons (reada env ra) (readb env rb)


--- WUI combinator for triples.
wTriple :: (Data a, Data b, Data c) =>
           WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec (a,b,c)
-- This simple implementation does not work in KiCS2 due to non-determinism
-- cause by functional patterns:
--wTriple = wCons3 (\a b c -> (a,b,c))
wTriple (WuiSpec wparamsa showa cora reada)
        (WuiSpec wparamsb showb corb readb)
        (WuiSpec wparamsc showc corc readc) =
  WuiSpec (renderTuple, tupleError, const True) showd cord readd
 where
  showd (render,errmsg,legal) nocheck (va,vb,vc) =
    let (hea,rta) = showa wparamsa nocheck va
        (heb,rtb) = showb wparamsb nocheck vb
        (hec,rtc) = showc wparamsc nocheck vc
     in ((if nocheck || legal (va,vb,vc)
            then render
            else renderError render errmsg) [hea,heb,hec],
         states2state [rta,rtb,rtc])

  cord wparamsd (va,vb,vc) = conditionOf wparamsd (va,vb,vc) &&
                             cora wparamsa va &&
                             corb wparamsb vb &&
                             corc wparamsc vc

  readd env s =
    let [ra,rb,rc] = state2states s
    in (reada env ra, readb env rb, readc env rc)

--- WUI combinator for constructors of arity 3.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons3 :: (Data a, Data b, Data c, Data d) => (a -> b -> c -> d)
       -> WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d
wCons3 cons (WuiSpec wparamsa showa cora reada)
            (WuiSpec wparamsb showb corb readb)
            (WuiSpec wparamsc showc corc readc) =
  WuiSpec (renderTuple, tupleError, const True) showd cord readd
 where
  showd (render,errmsg,legal) nocheck vd | cons va vb vc =:<= vd =
    let (hea,rta) = showa wparamsa nocheck va
        (heb,rtb) = showb wparamsb nocheck vb
        (hec,rtc) = showc wparamsc nocheck vc
     in ((if nocheck || legal (cons va vb vc)
            then render
            else renderError render errmsg) [hea,heb,hec],
         states2state [rta,rtb,rtc])
   where va,vb,vc free

  cord wparamsd vd | cons va vb vc =:<= vd =
    conditionOf wparamsd (cons va vb vc) &&
    cora wparamsa va &&
    corb wparamsb vb &&
    corc wparamsc vc            where va,vb,vc free

  readd env s =
    let [ra,rb,rc] = state2states s
    in cons (reada env ra) (readb env rb) (readc env rc)


--- WUI combinator for tuples of arity 4.
w4Tuple :: (Data a, Data b, Data c, Data d) => WuiSpec a -> WuiSpec b -> WuiSpec c
                                    -> WuiSpec d -> WuiSpec (a,b,c,d)
--w4Tuple = wCons4 (\a b c d -> (a,b,c,d)) -- does not work for KiCS2
w4Tuple wa wb wc wd =
  transformWSpec (\ ((a,b),(c,d)) -> (a,b,c,d),
                  \ (a,b,c,d) -> ((a,b),(c,d)))
                 (wJoinTuple (wPair wa wb) (wPair wc wd))


--- WUI combinator for constructors of arity 4.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons4  :: (Data a, Data b, Data c, Data d, Data e) => (a->b->c->d->e) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e
wCons4 cons wa wb wc wd =
  adaptWSpec (\ ((a,b),(c,d)) -> cons a b c d)
             (wJoinTuple (wPair wa wb) (wPair wc wd))


--- WUI combinator for tuples of arity 5.
w5Tuple :: (Data a, Data b, Data c, Data d, Data e) => WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
           WuiSpec (a,b,c,d,e)
--w5Tuple = wCons5 (\a b c d e -> (a,b,c,d,e)) -- does not work for KiCS2
w5Tuple wa wb wc wd we =
  transformWSpec (\ ((a,b,c),(d,e)) -> (a,b,c,d,e),
                  \ (a,b,c,d,e) -> ((a,b,c),(d,e)))
             (wJoinTuple (wTriple wa wb wc) (wPair wd we))

--- WUI combinator for constructors of arity 5.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons5  :: (Data a, Data b, Data c, Data d, Data e, Data f) => (a->b->c->d->e->f) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f
wCons5 cons wa wb wc wd we =
  adaptWSpec (\ ((a,b,c),(d,e)) -> cons a b c d e)
             (wJoinTuple (wTriple wa wb wc) (wPair wd we))


--- WUI combinator for tuples of arity 6.
w6Tuple :: (Data a, Data b, Data c, Data d, Data e, Data f) => WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
           WuiSpec f -> WuiSpec (a,b,c,d,e,f)
--w6Tuple = wCons6 (\a b c d e f -> (a,b,c,d,e,f))
w6Tuple wa wb wc wd we wf =
  transformWSpec (\ ((a,b,c),(d,e,f)) -> (a,b,c,d,e,f),
                  \ (a,b,c,d,e,f) -> ((a,b,c),(d,e,f)))
             (wJoinTuple (wTriple wa wb wc) (wTriple wd we wf))

--- WUI combinator for constructors of arity 6.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons6  :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g) =>
           (a->b->c->d->e->f->g) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g
wCons6 cons wa wb wc wd we wf =
  adaptWSpec (\ ((a,b,c),(d,e,f)) -> cons a b c d e f)
             (wJoinTuple (wTriple wa wb wc) (wTriple wd we wf))


--- WUI combinator for tuples of arity 7.
w7Tuple :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g) => WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
           WuiSpec f -> WuiSpec g -> WuiSpec (a,b,c,d,e,f,g)
--w7Tuple = wCons7 (\a b c d e f g -> (a,b,c,d,e,f,g))
w7Tuple wa wb wc wd we wf wg =
  transformWSpec (\ ((a,b,c,d),(e,f,g)) -> (a,b,c,d,e,f,g),
                  \ (a,b,c,d,e,f,g) -> ((a,b,c,d),(e,f,g)))
             (wJoinTuple (w4Tuple wa wb wc wd) (wTriple we wf wg))

--- WUI combinator for constructors of arity 7.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons7  :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h) =>
           (a->b->c->d->e->f->g->h) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h
wCons7 cons wa wb wc wd we wf wg =
  adaptWSpec (\ ((a,b,c,d),(e,f,g)) -> cons a b c d e f g)
             (wJoinTuple (w4Tuple wa wb wc wd) (wTriple we wf wg))


--- WUI combinator for tuples of arity 8.
w8Tuple :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h) => WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
           WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec (a,b,c,d,e,f,g,h)
--w8Tuple = wCons8 (\a b c d e f g h -> (a,b,c,d,e,f,g,h))
w8Tuple wa wb wc wd we wf wg wh =
  transformWSpec (\ ((a,b,c,d),(e,f,g,h)) -> (a,b,c,d,e,f,g,h),
                  \ (a,b,c,d,e,f,g,h) -> ((a,b,c,d),(e,f,g,h)))
             (wJoinTuple (w4Tuple wa wb wc wd) (w4Tuple we wf wg wh))

--- WUI combinator for constructors of arity 8.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons8 :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i) =>
          (a->b->c->d->e->f->g->h->i) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i
wCons8 cons wa wb wc wd we wf wg wh =
  adaptWSpec (\ ((a,b,c,d),(e,f,g,h)) -> cons a b c d e f g h)
             (wJoinTuple (w4Tuple wa wb wc wd) (w4Tuple we wf wg wh))


--- WUI combinator for tuples of arity 9.
w9Tuple :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i) => WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
           WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i ->
           WuiSpec (a,b,c,d,e,f,g,h,i)
--w9Tuple = wCons9 (\a b c d e f g h i -> (a,b,c,d,e,f,g,h,i))
w9Tuple wa wb wc wd we wf wg wh wi =
  transformWSpec (\ ((a,b,c,d,e),(f,g,h,i)) -> (a,b,c,d,e,f,g,h,i),
                  \ (a,b,c,d,e,f,g,h,i) -> ((a,b,c,d,e),(f,g,h,i)))
             (wJoinTuple (w5Tuple wa wb wc wd we) (w4Tuple wf wg wh wi))

--- WUI combinator for constructors of arity 9.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons9  :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j) => (a->b->c->d->e->f->g->h->i->j) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j
wCons9 cons wa wb wc wd we wf wg wh wi =
  adaptWSpec (\ ((a,b,c,d,e),(f,g,h,i)) -> cons a b c d e f g h i)
             (wJoinTuple (w5Tuple wa wb wc wd we) (w4Tuple wf wg wh wi))


--- WUI combinator for tuples of arity 10.
w10Tuple :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j) => WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec (a,b,c,d,e,f,g,h,i,j)
--w10Tuple = wCons10 (\a b c d e f g h i j -> (a,b,c,d,e,f,g,h,i,j))
w10Tuple wa wb wc wd we wf wg wh wi wj =
  transformWSpec (\ ((a,b,c,d,e),(f,g,h,i,j)) -> (a,b,c,d,e,f,g,h,i,j),
                  \ (a,b,c,d,e,f,g,h,i,j) -> ((a,b,c,d,e),(f,g,h,i,j)))
             (wJoinTuple (w5Tuple wa wb wc wd we) (w5Tuple wf wg wh wi wj))

--- WUI combinator for constructors of arity 10.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons10  :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k) => (a->b->c->d->e->f->g->h->i->j->k) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec k
wCons10 cons wa wb wc wd we wf wg wh wi wj =
  adaptWSpec (\ ((a,b,c,d,e),(f,g,h,i,j)) -> cons a b c d e f g h i j)
             (wJoinTuple (w5Tuple wa wb wc wd we) (w5Tuple wf wg wh wi wj))


--- WUI combinator for tuples of arity 11.
w11Tuple :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k) => WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec k -> WuiSpec (a,b,c,d,e,f,g,h,i,j,k)
--w11Tuple = wCons11 (\a b c d e f g h i j k -> (a,b,c,d,e,f,g,h,i,j,k))
w11Tuple wa wb wc wd we wf wg wh wi wj wk =
  transformWSpec (\ ((a,b,c,d,e),(f,g,h,i,j,k)) -> (a,b,c,d,e,f,g,h,i,j,k),
                  \ (a,b,c,d,e,f,g,h,i,j,k) -> ((a,b,c,d,e),(f,g,h,i,j,k)))
             (wJoinTuple (w5Tuple wa wb wc wd we) (w6Tuple wf wg wh wi wj wk))

--- WUI combinator for constructors of arity 11.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons11 :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l) =>
           (a->b->c->d->e->f->g->h->i->j->k->l) ->
           WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
           WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
           WuiSpec k -> WuiSpec l
wCons11 cons wa wb wc wd we wf wg wh wi wj wk =
  adaptWSpec (\ ((a,b,c,d,e),(f,g,h,i,j,k)) -> cons a b c d e f g h i j k)
             (wJoinTuple (w5Tuple wa wb wc wd we) (w6Tuple wf wg wh wi wj wk))


--- WUI combinator for tuples of arity 12.
w12Tuple :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h,
             Data i, Data j, Data k, Data l) =>
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec k -> WuiSpec l -> WuiSpec (a,b,c,d,e,f,g,h,i,j,k,l)
--w12Tuple = wCons12 (\a b c d e f g h i j k l -> (a,b,c,d,e,f,g,h,i,j,k,l))
w12Tuple wa wb wc wd we wf wg wh wi wj wk wl =
  transformWSpec (\ ((a,b,c,d,e,f),(g,h,i,j,k,l)) -> (a,b,c,d,e,f,g,h,i,j,k,l),
                  \ (a,b,c,d,e,f,g,h,i,j,k,l) -> ((a,b,c,d,e,f),(g,h,i,j,k,l)))
       (wJoinTuple (w6Tuple wa wb wc wd we wf) (w6Tuple wg wh wi wj wk wl))

--- WUI combinator for constructors of arity 12.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons12  :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h,
             Data i, Data j, Data k, Data l, Data m) =>
            (a->b->c->d->e->f->g->h->i->j->k->l->m) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec k -> WuiSpec l -> WuiSpec m
wCons12 cons wa wb wc wd we wf wg wh wi wj wk wl =
  adaptWSpec (\ ((a,b,c,d,e,f),(g,h,i,j,k,l)) -> cons a b c d e f g h i j k l)
       (wJoinTuple (w6Tuple wa wb wc wd we wf) (w6Tuple wg wh wi wj wk wl))

--- WUI combinator for tuples of arity 13.
w13Tuple :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h,
             Data i, Data j, Data k, Data l, Data m) =>
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec k -> WuiSpec l -> WuiSpec m ->
            WuiSpec (a,b,c,d,e,f,g,h,i,j,k,l,m)
--w13Tuple = wCons13 (\a b c d e f g h i j k l m -> (a,b,c,d,e,f,g,h,i,j,k,l,m))
w13Tuple wa wb wc wd we wf wg wh wi wj wk wl wm =
  transformWSpec
    (\ ((a,b,c,d,e,f),(g,h,i,j,k,l,m)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m),
     \ (a,b,c,d,e,f,g,h,i,j,k,l,m) -> ((a,b,c,d,e,f),(g,h,i,j,k,l,m)))
    (wJoinTuple (w6Tuple wa wb wc wd we wf) (w7Tuple wg wh wi wj wk wl wm))

--- WUI combinator for constructors of arity 13.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons13  :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h,
             Data i, Data j, Data k, Data l, Data m, Data n) =>
            (a->b->c->d->e->f->g->h->i->j->k->l->m->n) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec k -> WuiSpec l -> WuiSpec m -> WuiSpec n
wCons13 cons wa wb wc wd we wf wg wh wi wj wk wl wm =
  adaptWSpec
    (\ ((a,b,c,d,e,f),(g,h,i,j,k,l,m)) -> cons a b c d e f g h i j k l m)
    (wJoinTuple (w6Tuple wa wb wc wd we wf) (w7Tuple wg wh wi wj wk wl wm))

--- WUI combinator for tuples of arity 14.
w14Tuple :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h,
             Data i, Data j, Data k, Data l, Data m, Data n) =>
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec k -> WuiSpec l -> WuiSpec m -> WuiSpec n ->
            WuiSpec (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
--w14Tuple = wCons14 (\a b c d e f g h i j k l m n -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n))
w14Tuple wa wb wc wd we wf wg wh wi wj wk wl wm wn =
  transformWSpec
    (\ ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n),
     \ (a,b,c,d,e,f,g,h,i,j,k,l,m,n) -> ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n)))
    (wJoinTuple (w7Tuple wa wb wc wd we wf wg) (w7Tuple wh wi wj wk wl wm wn))

--- WUI combinator for constructors of arity 14.
--- The first argument is the ternary constructor.
--- The further arguments are the WUI specifications for the argument types.
wCons14  :: (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h,
             Data i, Data j, Data k, Data l, Data m, Data n, Data o) =>
            (a->b->c->d->e->f->g->h->i->j->k->l->m->n->o) ->
            WuiSpec a -> WuiSpec b -> WuiSpec c -> WuiSpec d -> WuiSpec e ->
            WuiSpec f -> WuiSpec g -> WuiSpec h -> WuiSpec i -> WuiSpec j ->
            WuiSpec k -> WuiSpec l -> WuiSpec m -> WuiSpec n -> WuiSpec o
wCons14 cons wa wb wc wd we wf wg wh wi wj wk wl wm wn =
  adaptWSpec
    (\ ((a,b,c,d,e,f,g),(h,i,j,k,l,m,n)) -> cons a b c d e f g h i j k l m n)
    (wJoinTuple (w7Tuple wa wb wc wd we wf wg) (w7Tuple wh wi wj wk wl wm wn))


--- WUI combinator to combine two tuples into a joint tuple.
--- It is similar to wPair but renders both components as a single
--- tuple provided that the components are already rendered as tuples,
--- i.e., by the rendering function <code>renderTuple</code>.
--- This combinator is useful to define combinators for large tuples.
wJoinTuple :: (Data a, Data b) => WuiSpec a -> WuiSpec b -> WuiSpec (a,b)
wJoinTuple (WuiSpec wparamsa showa cora reada)
           (WuiSpec wparamsb showb corb readb) =
  WuiSpec (renderTuple, tupleError, const True) showc corc readc
 where
  render2joinrender render [h1,h2] =
    let h1s = unRenderTuple h1
        h2s = unRenderTuple h2
     in render (h1s++h2s)

  showc (render,errmsg,legal) nocheck (va,vb) =
    let (hea,rta) = showa wparamsa nocheck va
        (heb,rtb) = showb wparamsb nocheck vb
     in ((if nocheck || legal (va,vb)
            then render2joinrender render
            else renderError (render2joinrender render) errmsg) [hea,heb],
         states2state [rta,rtb])

  corc wparamsc (va,vb) = conditionOf wparamsc (va,vb) &&
                          cora wparamsa va && corb wparamsb vb

  readc env s =
    let [ra,rb] = state2states s
    in (reada env ra, readb env rb)


--- WUI combinator for list structures where the list elements are vertically
--- aligned in a table.
wList :: Eq a => WuiSpec a -> WuiSpec [a]
wList (WuiSpec wparamsa showa cora reada) =
  WuiSpec (renderList, "Illegal list:", const True)
          (\ (render,errmsg,legal) nocheck vas ->
             listWidget
               (if nocheck || legal vas
                  then render
                  else renderError render errmsg)
               (unzip (map (showa wparamsa nocheck) vas)))
          (\wparams vas -> conditionOf wparams vas &&
                           all (cora wparamsa) vas)
          (\env wst -> map (reada env) (state2states wst))
 where
  listWidget render (hes,refs) = (render hes, states2state refs)

--- Add headings to a standard WUI for list structures:
wListWithHeadings :: Eq a => [String] -> WuiSpec a -> WuiSpec [a]
wListWithHeadings headings wspec =
  wList wspec `withRendering` renderHeadings
 where
  renderHeadings hs = addHeadings (renderList hs) (map (\s->[htxt s]) headings)

--- Add a row of items (where each item is a list of HTML expressions)
--- as headings to a table. If the first argument is not a table,
--- the headings are ignored.
addHeadings :: HtmlExp -> [[HtmlExp]] -> HtmlExp
addHeadings htable headings = case htable of
   HtmlStruct "table" attrs rows ->
     HtmlStruct "table" attrs
       (HtmlStruct "tr" [] (map (HtmlStruct "th" []) headings) : rows)
   _ -> htable

--- WUI combinator for list structures where the list elements are horizontally
--- aligned in a table.
wHList :: Eq a => WuiSpec a -> WuiSpec [a]
wHList wspec = wList wspec `withRendering` renderTuple


--- WUI for matrices, i.e., list of list of elements
--- visualized as a matrix.
wMatrix :: Eq a => WuiSpec a -> WuiSpec [[a]]
wMatrix wspec = wList (wHList wspec)


--- WUI for Maybe values. It is constructed from a WUI for
--- Booleans and a WUI for the potential values. Nothing corresponds
--- to a selection of False in the Boolean WUI.
--- The value WUI is shown after the Boolean WUI.
--- @param wspecb - a WUI specification for Boolean values
--- @param wspeca - a WUI specification for the type of potential values
--- @param def - a default value that is used if the current value is Nothing
wMaybe :: Eq a => WuiSpec Bool -> WuiSpec a -> a -> WuiSpec (Maybe a)
wMaybe (WuiSpec paramb showb _ readb) (WuiSpec parama showa cora reada) def =
 WuiSpec
   (renderTuple, tupleError, const True)
   (\ (render,errmsg,legal) nocheck mbs ->
     let (heb,rtb) = showb paramb nocheck (mbs/=Nothing)
         (hea,rta) = showa parama nocheck (maybe def id mbs)
      in ((if nocheck || legal mbs
             then render
             else renderError render errmsg) [heb,hea],
          states2state [rtb,rta]))
   (\wparams mbv -> conditionOf wparams mbv &&
                    maybe True (\v -> cora parama v) mbv)
   (\env s ->
     let [rb,ra] = state2states s
         vb = readb env rb
         va = reada env ra
      in if vb then Just va else Nothing)

--- A WUI for Maybe values where a check box is used to select Just.
--- The value WUI is shown after the check box.
--- @param wspec - a WUI specification for the type of potential values
--- @param hexps - a list of HTML expressions shown after the check box
--- @param def - a default value if the current value is Nothing
wCheckMaybe :: Eq a => WuiSpec a -> [HtmlExp] -> a -> WuiSpec (Maybe a)
wCheckMaybe wspec exps = wMaybe (wCheckBool exps) wspec

--- A WUI for Maybe values where radio buttons are used to switch
--- between Nothing and Just.
--- The value WUI is shown after the radio button WUI.
--- @param wspec - a WUI specification for the type of potential values
--- @param hexps - a list of HTML expressions shown after the Nothing button
--- @param hexps - a list of HTML expressions shown after the Just button
--- @param def - a default value if the current value is Nothing
wRadioMaybe :: Eq a => WuiSpec a -> [HtmlExp] -> [HtmlExp] -> a
                    -> WuiSpec (Maybe a)
wRadioMaybe wspec hnothing hjust = wMaybe wBool wspec
 where
  wBool = wRadioSelect (\b->if b then hjust else hnothing) [False,True]


--- WUI for union types.
--- Here we provide only the implementation for Either types
--- since other types with more alternatives can be easily reduced to this case.
wEither :: (Eq a, Eq b) => WuiSpec a -> WuiSpec b -> WuiSpec (Either a b)
wEither (WuiSpec rendera showa cora reada) (WuiSpec renderb showb corb readb) =
 WuiSpec (head, "?", const True) showEither corEither readEither
 where
  showEither (render,errmsg,legal) nocheck (Left va) =
    let (hea,rta) = showa rendera nocheck va
     in ((if nocheck || legal (Left va)
            then render
            else renderError render errmsg) [hea], altstate2state (1,rta))
  showEither (render,errmsg,legal) nocheck (Right vb) =
    let (heb,rtb) = showb renderb nocheck vb
     in ((if nocheck || legal (Right vb)
            then render
            else renderError render errmsg) [heb], altstate2state (2,rtb))

  corEither wparam vab = conditionOf wparam vab &&
                         either (cora rendera) (corb renderb) vab

  readEither env s =
    let (altindex,rab) = state2altstate s
     in case altindex of
          1 -> Left  (reada env rab)
          2 -> Right (readb env rab)

--- A simple tree structure to demonstrate the construction of WUIs for tree
--- types.
data WTree a = WLeaf a | WNode [WTree a]
 deriving Eq

--- WUI for tree types.
--- The rendering specifies the rendering of inner nodes.
--- Leaves are shown with their default rendering.
wTree :: Eq a => WuiSpec a -> WuiSpec (WTree a)
wTree (WuiSpec wparama showa cora reada) =
  WuiSpec (renderList, "Illegal tree:", const True) showTree corTree readTree
 where
  showTree (render,errmsg,legal) nocheck (WLeaf va) =
    let (hea,rta) = showa wparama nocheck va
     in ((if nocheck || legal (WLeaf va)
                             then render
                             else renderError render errmsg) [hea],
         altstate2state (1,rta))
  showTree wparams@(render,errmsg,legal) nocheck (WNode ns) =
    let (hes,sts) = unzip (map (showTree wparams nocheck) ns)
     in ((if nocheck || legal (WNode ns)
            then render
            else renderError render errmsg) hes,
         altstate2state (2,states2state sts))

  corTree wparam (WLeaf va)  = conditionOf wparam (WLeaf va) && cora wparama va
  corTree wparam (WNode tvs) = conditionOf wparam (WNode tvs) &&
                               all (corTree wparam) tvs

  readTree env wst =
    let (altindex,rab) = state2altstate wst
     in case altindex of
         1 -> WLeaf (reada env rab)
         2 -> WNode (map (readTree env) (state2states rab))

-------------------------------------------------------------------------------
-- Definition of standard rendering functions

--- Standard rendering of tuples as a table with a single row.
--- Thus, the elements are horizontally aligned.
renderTuple :: Rendering
renderTuple hexps = table [map (\h->[h]) hexps]

--- Inverse operation of renderTuple. If the argument has not the
--- shape of the renderTuple output, it is returned unchanged.
--- In future versions, this operation is better implemented using
--- functional logic features, but currently the encapsulated search
--- is a bit weak for this purpose.
unRenderTuple :: HtmlExp -> [HtmlExp]
unRenderTuple hexp =
  if isTupleTable hexp
  then getTupleTableElems hexp
  else [hexp]
 where
  isTupleTable he = case he of
    HtmlStruct "table" [] [HtmlStruct "tr" [] tds] -> all isSingleElem tds
    _ -> False

  isSingleElem he = case he of
    HtmlStruct "td" _ [_] -> True
    _ -> False

  getTupleTableElems (HtmlStruct "table" [] [HtmlStruct "tr" [] tds]) =
    map (\ (HtmlStruct "td" _ [e]) -> e) tds

-- Standard error message for tuples:
tupleError :: String
tupleError = "Illegal combination:"

--- Standard rendering of tuples with a tag for each element.
--- Thus, each is preceded by a tag, that is set in bold, and all
--- elements are vertically aligned.
renderTaggedTuple :: [String] -> Rendering
renderTaggedTuple tags hexps =
  table (map (\(t,h)->[[bold [htxt t]],[h]]) (zip tags hexps))

--- Standard rendering of lists as a table with a row for each item:
--- Thus, the elements are vertically aligned.
renderList :: Rendering
renderList hexps = mergeTableOfTable (table (map (\h->[[h]]) hexps))
                                                `addAttr` ("border","1")

-- Combine a rendering with an error message.
-- The error message is put as the first row of a table with background color
-- yellow.
renderError :: Rendering -> String -> Rendering
renderError render errmsg hexps =
  table [[[boldRedTxt errmsg]], [[render hexps]]] 
                  `addAttr` ("bgcolor","#ffff00") -- background color: yellow

boldRedTxt :: String -> HtmlExp
boldRedTxt s = HtmlStruct "font" [("color","#ff0000")] [bold [htxt s]]


mergeTableOfTable :: HtmlExp -> HtmlExp
mergeTableOfTable (HtmlStruct "table" attrs rows) =
  HtmlStruct "table" attrs
             (if all isRowWithSingleTableData rows
              then map mergeRowWithSingleTableData rows
              else rows )

isRowWithSingleTableData :: HtmlExp -> Bool
isRowWithSingleTableData row = case row of
   (HtmlStruct "tr" []
        [HtmlStruct "td" []
            [HtmlStruct "table" _ [HtmlStruct "tr" _ _]]]) -> True
   _ -> False

mergeRowWithSingleTableData :: HtmlExp -> HtmlExp
mergeRowWithSingleTableData 
  (HtmlStruct "tr" [] [HtmlStruct "td" [] [HtmlStruct "table" _ [row]]]) = row


-------------------------------------------------------------------------------
--- The type of data actually stored in a WUI store.
--- If the first component is `True`, the current data is not immediately
--- checked for correctness (usually, if it is the first edit call).
--- The second component is `Nothing` if the data is not yet set.
type WuiStore a = (Bool, Maybe a)

--- A `WuiSessionStore` is a persistent global entity to store the
--- information required for WUIs in HTML sessions.
type WuiSessionStore a = SessionStore (WuiStore a)

--- A `ParWuiSessionStore b a` is a persistent global entity to store the
--- information required for WUIs in HTML sessions which manipulates
--- data of type `a` and depend on additional information of type `b`.
type ParWuiSessionStore b a = SessionStore (b, WuiStore a)

--- Sets the initial data which are edited in a WUI form in the session store.
setWuiStore :: (Read a, Show a) => WuiSessionStore a -> a -> IO ()
setWuiStore wuistore val = putSessionData wuistore (True, Just val)

--- Reads the data which are edited in a WUI form from the session store.
getWuiStore :: (Read a, Show a) =>
               WuiSessionStore a -> FormReader (WuiStore a)
getWuiStore wuistore = getSessionData wuistore (True, Nothing)

--- Sets the initial data which are edited in a parameterized WUI form
--- in the session store.
setParWuiStore :: (Read a, Show a, Read b, Show b) =>
                  ParWuiSessionStore b a -> b -> a -> IO ()
setParWuiStore wuistore par val =
  putSessionData wuistore (par, (True, Just val))

--- Reads the data which are edited in a parameterized WUI form
--- from the session store.
getParWuiStore :: (Read a, Show a, Read b, Show b) =>
                  ParWuiSessionStore b a -> FormReader (b,WuiStore a)
getParWuiStore wuistore = getSessionData wuistore (failed, (True, Nothing))

-- Main operations to generate HTML form definitions from WUI specifications:

--- Generates an HTML form definition from a string (the qualified name
--- of the top-level operation corresponding to this form),
--- a session data store containing the data to be edited,
--- a WUI specification,
--- an action to store the updated data and returning an HTML answer,
--- an operation to render the WUI (e.g., `wuiSimpleRenderer`), and
--- which is used when input errors must be corrected,
--- from the HTML WUI expression and submit handler.
wui2FormDef :: (Read a, Show a) =>
               String
            -> WuiSessionStore a
            -> WuiSpec a
            -> (a -> IO [BaseHtml])
            -> (HtmlExp -> (HtmlEnv -> IO [BaseHtml]) -> [HtmlExp])
            -> HtmlFormDef (WuiStore a)
wui2FormDef formqname wuistore wuispec storepage renderwui =
  let wuiformdef = formDefWithID formqname (getWuiStore wuistore)
                                 (formHtml wuiformdef)
  in wuiformdef
 where
  formHtml iform sdata =
    wui2HtmlExp wuistore wuispec storepage renderwui iform sdata

--- Generates an HTML form expression from
--- a session store containing the data to be edited,
--- a WUI data specification,
--- an action to store the updated data and returning an HTML answer,
--- an operation to render the WUI (e.g., `wuiSimpleRenderer`),
--- which is used when input errors must be corrected,
--- an HTML form definition representing the generated form,
--- and the actual data of the store.
wui2HtmlExp :: (Read a, Show a) =>
               WuiSessionStore a
            -> WuiSpec a
            -> (a -> IO [BaseHtml])
            -> (HtmlExp -> (HtmlEnv -> IO [BaseHtml]) -> [HtmlExp])
            -> HtmlFormDef (WuiStore a)
            -> WuiStore a -> [HtmlExp]
wui2HtmlExp _ _ _ _ _ (_,Nothing) =
  [h1 [htxt "Execution error!"],
   htxt "Cookie not yet set, please run again or accept cookies!"]
wui2HtmlExp wuistore (WuiSpec wparams wshow wcor wread) storepage renderwui
            wuiformdef (fstcall,Just val) =
  let (hexp,wst) = wshow wparams fstcall val
  in renderwui hexp (handler wst)
 where
  handler wst env = do
    let newval = id $## wread env wst -- ensure that everything is evaluated
    if (wcor wparams) newval
      then do putSessionData wuistore (True, Nothing)
              storepage newval
      else do putSessionData wuistore (False, Just newval)
              return [formElem wuiformdef]


--- Generates an HTML form definition similarly to `wui2FormDef`
--- but with some additional data on which the further arguments depend.
pwui2FormDef :: (Read a, Show a, Read b, Show b) =>
                String
             -> ParWuiSessionStore b a
             -> (b -> WuiSpec a)
             -> (b -> a -> IO [BaseHtml])
             -> (b -> HtmlExp -> (HtmlEnv -> IO [BaseHtml]) -> [HtmlExp])
             -> HtmlFormDef (b, WuiStore a)
pwui2FormDef formqname wuistore wuispec storepage renderwui =
  let wuiformdef = formDefWithID formqname (getParWuiStore wuistore)
                                 (formHtml wuiformdef)
  in wuiformdef
 where
  formHtml iform sdata =
    pwui2HtmlExp wuistore wuispec storepage renderwui iform sdata

--- Generates an HTML form expression similarly to `wui2HtmlExp`
--- but with some additional data on which the further arguments depend.
pwui2HtmlExp :: (Read a, Show a, Read b, Show b) =>
                ParWuiSessionStore b a
             -> (b -> WuiSpec a)
             -> (b -> a -> IO [BaseHtml])
             -> (b -> HtmlExp -> (HtmlEnv -> IO [BaseHtml]) -> [HtmlExp])
             -> HtmlFormDef (b,WuiStore a)
             -> (b, WuiStore a) -> [HtmlExp]
pwui2HtmlExp _ _ _ _ _ (_,(_,Nothing)) =
  [h1 [htxt "Execution error!"],
   htxt "Cookie not yet set, please run again or accept cookies!"]
pwui2HtmlExp wuistore pwuispec storepage renderwui
            wuiformdef (par, (fstcall,Just val)) =
  let (WuiSpec wparams wshow wcor wread) = pwuispec par
      (hexp,wst) = wshow wparams fstcall val
  in renderwui par hexp (handler wparams wcor wread wst)
 where
  handler wparams wcor wread wst env = do
    let newval = id $## wread env wst -- ensure that everything is evaluated
    if (wcor wparams) newval
      then do putSessionData wuistore (par, (True, Nothing))
              storepage par newval
      else do putSessionData wuistore (par, (False, Just newval))
              return [formElem wuiformdef]

--- A standard rendering for WUI forms.
--- The arguments are the HTML expression representing the WUI fields
--- and the handler for the "submit" button.
wuiSimpleRenderer :: HtmlExp -> (HtmlEnv -> IO [BaseHtml]) -> [HtmlExp]
wuiSimpleRenderer inputhexp storehandler =
  [inputhexp, breakline,
   button "Submit" (\env -> storehandler env >>= return . page "Answer")]

--------------------------------------------------------------------------

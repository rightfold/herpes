module Herpes.Form.Generic
  ( gformlet
  ) where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Herpes.Form (Form, textField, labeledForm)
import Generics.SOP


class HasFormlet a where
  formlet :: a -> Form a

instance HasFormlet Text where
  formlet = textField


getInfo :: DatatypeInfo xss -> NP ConstructorInfo xss
getInfo (ADT _ _ xs) = xs
getInfo (Newtype _ _ x) = x :* Nil

gformlet :: forall a xs.
  ( Generic a
  , HasDatatypeInfo a
  , All2 HasFormlet (Code a)
  , Code a ~ '[xs]
  ) => a -> Form a
gformlet = fmap to . gformlet' (datatypeInfo (Proxy @a)) . from

gformlet' :: (All2 HasFormlet xss, xss ~ '[xs]) => DatatypeInfo xss -> SOP I xss -> Form (SOP I xss)
gformlet' _ (SOP (S xss)) = case xss of {} 
gformlet' info (SOP (Z xs)) =
  case getInfo info of
    (cInfo :* Nil) -> SOP . Z <$> gformlet'' cInfo xs

gformlet'' :: All HasFormlet xs => ConstructorInfo xs -> NP I xs -> Form (NP I xs)
gformlet'' (Constructor _) xs = 
  hctraverse' (Proxy @HasFormlet) (traverse formlet) xs
gformlet'' (Infix _ _ _) xs = 
  hctraverse' (Proxy @HasFormlet) (traverse formlet) xs
gformlet'' (Record _ fields) xs =
  hsequence $ hcliftA2 (Proxy @HasFormlet) (\(FieldInfo name) (I a) -> labeledForm (Text.pack name) (formlet a)) fields xs







module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = case planet of
    Mercury -> life / 0.2408467
    Venus -> life / 0.61519726
    Earth -> life
    Mars -> life / 1.8808158
    Jupiter -> life / 11.862615
    Saturn -> life / 29.447498
    Uranus -> life / 84.016846
    Neptune -> life / 164.79132
    where
      life = seconds / 31557600

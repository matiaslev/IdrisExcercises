
data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
     Unicycles : Vehicle Pedal
     Bicycle : Vehicle Pedal
     Motorcycle : (fuel : Nat) -> Vehicle Petrol
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol
     Tram : Vehicle Electric
     ElectricCar : Vehicle Electric

wheels : Vehicle power -> Nat
wheels Unicycles = 1
wheels Bicycle = 2
wheels Motorcycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels Tram = 0
wheels ElectricCar = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 50
-- optional explicit
refuel Bicycle impossible

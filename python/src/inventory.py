from .simulation import Simulation
import copy

class Lot:
    def __init__(self, id, sim: Simulation, size, thc, leadtime):
        self.id = id
        self.sim = sim #reference to instance of sim object for date centralization (composition)
        self.size = size
        self.thc = thc
        self.leadtime = leadtime

        #new attributes
        self.qtyavailable = size
        self.qtysold = 0
        self.dateordered = self.sim.date
        self.dateavailable = self.dateordered + self.leadtime

    def __deepcopy__(self, memo):
        # Note: sim must be deepcopied externally before Inventory copies lots
        new_lot = Lot(self.id, sim=memo['sim'], size=self.size, thc=self.thc, leadtime=self.leadtime)
        new_lot.qtyavailable = self.qtyavailable
        new_lot.qtysold = self.qtysold
        new_lot.dateordered = self.dateordered
        new_lot.dateavailable = self.dateavailable
        return new_lot

    
    #Methods
    def makesale(self, qty):
        if not self.available:
            raise Exception(f"Lot unavailable until {self.dateavailable}")
        elif self.qtyavailable < qty:
            raise Exception(f"Requesting {qty} but only {self.qtyavailable} available")
        else:
            self.qtyavailable -= qty
            self.qtysold += qty

    @property
    def age(self):
        return self.sim.date - self.dateavailable
    
    @property
    def available(self):
        return self.sim.date >= self.dateavailable

    def __repr__(self):
        return f"Lot(id={self.id}, size={self.size}, thc={self.thc}, leadtime={self.leadtime}, dateordered={self.dateordered})"
    

class Inventory:
    def __init__(self, sim: Simulation):
        self.sim = sim
        self.lots = [] #list of Lot objects

    def __deepcopy__(self, memo):
        new_sim = copy.deepcopy(self.sim, memo) #copy the sim object
        memo['sim'] = new_sim  # So each Lot uses the same copied sim
        new_inv = Inventory(sim=new_sim)
        new_inv.lots = [copy.deepcopy(lot, memo) for lot in self.lots]
        return new_inv

    def replenish(self, qty: int, thc: float, leadtime: int):
        #create a new lot and add it to the inventory
        lot = Lot(id=len(self.lots), sim=self.sim, size=qty, thc=thc, leadtime=leadtime)
        self.lots.append(lot)
        return lot
    
    def qtyavailable(self, max_age: int):
        total = 0
        for lot in self.lots:
            if lot.available and lot.age <= max_age:
                total += lot.qtyavailable
        return total
    
    def sell_fifo(self, demand, fresh):
        #make a sale from the oldest lots
        self.lots.sort(key=lambda lot: lot.age, reverse=True) #oldest first
        return self.fill_order(demand, fresh)
        
    def sell_lifo(self, demand, fresh):
        #make a sale from the newest lots
        self.lots.sort(key=lambda lot: lot.age, reverse=False) #youngest first
        return self.fill_order(demand, fresh)

    def sell_bifo(self, demand, fresh):
        #make a sale from highest-thc lots
        self.lots.sort(key=lambda lot: lot.thc, reverse=True) #highest thc first
        return self.fill_order(demand, fresh)

    def fill_order(self, demand, fresh):
        unfilled = demand
        filled = 0
        for lot in self.lots:
            if lot.available and lot.age <= fresh:
                if lot.qtyavailable >= unfilled:
                    lot.makesale(unfilled)
                    filled += unfilled
                    unfilled = 0
                elif lot.qtyavailable > 0:
                    filled += lot.qtyavailable
                    unfilled -= lot.qtyavailable
                    lot.makesale(lot.qtyavailable) #must be after tallying sale
        return unfilled

    def print_inventory(self):
        for lot in self.lots:
            print(f"Lot ID: {lot.id:>3}, Qty Produced: {lot.size}, THC: {lot.thc}, Leadtime: {lot.leadtime}, Date Ordered: {lot.dateordered:>4}, Qty Available: {lot.qtyavailable:>5}, Qty Sold: {lot.qtysold:>5}, Date Available: {lot.dateavailable:>4}, Age: {lot.age:>4}")
        print(f"Lots produced =  {max(lot.id for lot in self.lots)}")
        print(f"Units produced = {sum(lot.size for lot in self.lots)}")
        print(f"Units sold =     {sum(lot.qtysold for lot in self.lots)}")
        eofrac = round((sum(lot.qtyavailable for lot in self.lots if lot.age > 50))/(sum(lot.size for lot in self.lots)),3)
        print(f"E&O fraction =   {eofrac}")
        return eofrac

    def __repr__(self):
        return f"Inventory(lots={self.lots})"
    


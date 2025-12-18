import numpy as np
from collections import defaultdict

def mround(x, multiple):
    return [round(n / multiple) * multiple for n in x]

class IntermittentDemand:
    """
    This class generates intermittent demand using a Poisson distribution for the number of purchase orders (POs)
    and a normal distribution for the units per PO.

    THIS IS A COMPOUND POISSON PROCESS.
    """

    def __init__(self, customer, rate, mean, stdev, fresh, fcbias = 0):
        self.customer = customer
        self.rate = rate # poisson rate, i.e. lambda
        self.mean = mean    # mean (normal distribution)
        self.stdev = stdev  # standard deviation (normal distribution)
        self.fresh = fresh
        self.fcbias = fcbias

    def generate(self, n, seed = None):
        # Returns total units demanded for n perids

        rng = np.random.default_rng(seed = seed) #random number generator

        pos = []
        pos_units = []
        pos_totals = []
        fcst = [self.rate * self.mean * (1 + self.fcbias)] * n #forecast demand, inclusive of bias

        for _ in range (n):
            #get pos per period (Poisson)
            count = rng.poisson(self.rate)
            pos.append(count)

            #get units per po (normal)
            if count > 0:
                po_units = mround(list(rng.normal(self.mean, self.stdev, size=count)), 6)
                po_units = [6 if x < 0 else x for x in po_units] #replace negative values with 6
            else:
                po_units = [0]

            pos_units.extend(po_units)
            
            #get total units
            pos_totals.append(sum(po_units))

        return {"demand": pos_totals, "forecast": fcst, "fresh": self.fresh, "customer": self.customer, "mean": self.rate * self.mean} #return as dictionary

    def variance_per_period(self):
        #variance of demand during lead time
        return self.rate * (self.mean**2 + self.stdev**2)

class AggregateDemand:
    #This class aggregates demand from multiple customers.

    def __init__(self):
        #this stores instances of IntermittentDemand.generate()
        #each entry in the list is a dictionary with keys: demand, forecast, fresh, customer, mean
        self.components = []

    def add(self, demand):
        self.components.append(demand)
    
    @property
    def aggregate_demand(self):
        # Returns total demand for all customers

        aggregated = defaultdict(lambda: np.zeros(len(self.components[0]["demand"]), dtype=int))

        for entry in self.components:
            aggregated[entry["fresh"]] += np.array(entry["demand"])
        
        result = {fresh: demand.tolist() for fresh, demand in aggregated.items()}
        return result
    
    @property
    def aggregate_forecast(self):
        # Returns total demand for all customers

        aggregated = defaultdict(lambda: np.zeros(len(self.components[0]["forecast"]), dtype=float))

        for entry in self.components:
            aggregated[entry["fresh"]] += np.array(entry["forecast"])
        
        result = {fresh: forecast.tolist() for fresh, forecast in aggregated.items()}
        return result
    
    @property
    def LCD_fresh(self):
        #lowest common demoniator for fresh requirements
        return min(component["fresh"] for component in self.components)
    
    @property
    def mean_total_demand(self):
        #total mean demand for all customers
        return sum(component["mean"] for component in self.components)
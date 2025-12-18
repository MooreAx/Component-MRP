import copy

class Simulation:
    def __init__(self, date=0):
        self.date = date
    
    def advance_time(self, increment=1):
        self.date += increment

    def __deepcopy__(self, memo):
        # Create a new instance of the class (i.e. a copy of it, not a reference)
        return Simulation(self.date)
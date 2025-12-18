import scipy.stats as st
from .demand import IntermittentDemand, AggregateDemand
from collections import defaultdict

def get_safetystock_1(intdmdlist, leadtime, service_level):
    #calculates safety stock for total demand, ignoring freshness requirement

    total_variance = 0
    total_mean = 0
    for component in intdmdlist:
        total_variance += component.variance_per_period()
        total_mean += component.mean * component.rate #mean demand per period

    #variance of total demand over the lead time:
    sigma_l = (total_variance * leadtime)**0.5
    mean_l = total_mean * leadtime

    #get zscore from service level
    z = st.norm.ppf(service_level)
    ss = z * sigma_l

    print("\n")
    print(f"Mean lead time demand =     {mean_l}")
    print(f"stdev lead time demand =    {sigma_l}")
    print(f"Z score =                   {z}")
    print(f"Safety stock =              {ss}")
    print("\n")
    
    return ss

def get_safetystock_2(intdmdlist, leadtime, service_level):
    #calculates safety stock per freshness requirement
    
    #group intdmdlist by freshness - creates a dict with freshness req't as key
    freshgroups = defaultdict(list)
    for d in intdmdlist:
        freshgroups[d.fresh].append(d)

    ss_by_fresh = {} #dict for storing results

    for fresh, components in freshgroups.items(): #freshgrop.items() returns a list of tuples: [(fresh, [list of components])]
        #calculate safetystock for this freshness group
        ss = get_safetystock_1(components, leadtime, service_level)

        ss_by_fresh[fresh] = ss
        print(f"Safety stock for freshness {fresh} = {ss}")
        print(ss_by_fresh)
    return ss_by_fresh


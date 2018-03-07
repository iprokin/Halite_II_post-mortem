#!/usr/bin/python3

from multiprocessing import Pool, TimeoutError, cpu_count
from subprocess import check_output
from time import sleep
import re

def play_once():
    sleep(0.1)
    s = check_output("./run_game.sh | grep \"rank #1\"", shell=True).decode("utf-8")
    m = re.search("(Player #[0-9])", s)
    if m is not None:
        n = m.group(1)
        print(n)
        return n
    else:
        return 'fail'

def play_onceI(_):
    return play_once()

def count(xs):
    d = {}
    for x in xs:
        if x in d.keys():
            d[x] += 1
        else:
            d[x] = 1
    return d

def win_rate(d):
    d = {k:d[k] for k in d.keys() if 'Player' in k}
    total = sum(d.values())
    return {k: "{}%".format(v/total*100) for k,v in d.items()}

if __name__ == '__main__':

    def run_parallel():
        pool = Pool(processes=cpu_count())
        res = []
        try:
            for r in pool.imap_unordered(play_onceI, range(100)):
                res.append(r)
                print(count(res))
            return res
        except KeyboardInterrupt:
            return res

    res = run_parallel()

    print("\n")
    print(win_rate(count(res)))

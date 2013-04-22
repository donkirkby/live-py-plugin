def search(n, a):
    low = 0
    high = len(a) - 1
    while low <= high:
        mid = (low + high) / 2
        v = a[mid]
        if n == v:
            return mid
        if n < v:
            high = mid - 1
        else:
            low = mid + 1
    return -1

if __name__ == '__live_coding__':
    i = search(4, [1, 2, 4])

if __name__ == '__main__':
    print search(4, [1, 2, 4])
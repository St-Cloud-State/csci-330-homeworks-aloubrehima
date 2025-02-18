#include <iostream>
#include <stack>
#include <vector>

using namespace std;

int partition(vector<int>& arr, int low, int high) { // Partition function for quicksort
    int pivot = arr[high]; // Choosing last element as pivot
    int i = low - 1; // Index of smaller element
    for (int j = low; j < high; j++) { // Traverse through all elements
        if (arr[j] < pivot) {  // If current element is smaller than the pivot
            i++;
            swap(arr[i], arr[j]); 
        }
    }
    swap(arr[i + 1], arr[high]);
    return i + 1;
}

void quicksort(vector<int>& arr) { // Iterative Quicksort function
    stack<pair<int, int>> stk;
    stk.push({0, arr.size() - 1}); // Push initial low and high indices

    while (!stk.empty()) {
        int low = stk.top().first, high = stk.top().second;
        stk.pop();
        
        if (low < high) {
            int p = partition(arr, low, high); // Get partition index

            // Push sub-arrays to stack
            stk.push({low, p - 1}); // Left sub -array
            stk.push({p + 1, high}); // Right sub-array
        }
    }
}

int main() {
    vector<int> arr = {4, 7, 10, 32, 5, 62, 32, 45};
    quicksort(arr);
    
    cout << "Sorted array: ";
    for (int num : arr) {
        cout << num << " ";
    }
    cout << endl;
    return 0;
}

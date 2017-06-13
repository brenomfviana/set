#include <iostream>

using namespace std;

int main(int argc, const char * argv[]) {

	int m = 3;
	int n = 3;
	int mxn = m * n;
	
	int x[mxn];
	int y[mxn];

	cout << "Lendo o vetor x: ";
	for(int i = 0; i < m; i++){
		for(int j = 0; j < n; j++){
			cin >> x [(3*i)+j];
		}
	}
	cout << endl;

	cout << "Lendo o vetor y: ";
	for(int i = 0; i < m; i++){
		for(int j = 0; j < n; j++){
			cin >> y[(3*i)+j];
		}
	}
	cout << endl;

	int soma[mxn];
	for(int i = 0; i < m; i++){
		for(int j = 0; j < n; j++){
			soma[(3*i)+j] = x[(3*i)+j] + y[(3*i)+j];
		}
	}


	for(int i = 0; i < m; i++){
		for(int j = 0; j < n; j++){
			cout << x[(3*i)+j] << " ";
		}
		cout << endl;
	}

	cout << "+" << endl;

	for(int i = 0; i < m; i++){
		for(int j = 0; j < n; j++){
			cout << y[(3*i)+j] << " ";
		}
		cout << endl;
	}

	cout << "=" << endl;

	for(int i = 0; i < m; i++){
		for(int j = 0; j < n; j++){
			cout << soma[(3*i)+j] << " ";
		}
		cout << endl;
	}

    
    return 0;
}
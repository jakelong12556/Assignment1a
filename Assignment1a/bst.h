#ifndef BST_H
#define BST_H
struct Node{
	int data;
	struct Node *left;
	struct Node *right;
};
typedef struct Node Node;

Node* insertNode(Node *root, int value);
Node* deleteNode(Node* root, int value);
void printSubtree(Node *N);
int countNodes(Node *N);
Node* freeSubtree(Node *N);

#endif

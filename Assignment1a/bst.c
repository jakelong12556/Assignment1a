#include<stdio.h>
#include<stdlib.h>
#include "bst.h"


Node* insertNode(Node *root, int value)
{
    if (root == NULL) {
        Node* newNode = (Node*)malloc(sizeof(struct Node)); 
        newNode->data = value;
        newNode->left = NULL;
        newNode->right = NULL;
        return newNode;
    } 

    if (value < root->data) {
        root->left = insertNode(root->left, value);
    } else {
        root->right = insertNode(root->right,value);
    }
    return root;
}

Node* deleteNode(Node* root, int value)
{
	if (root == NULL) {
		return root;
	}

	if (value < root->data) {
		root->left = deleteNode(root->left, value);
	}
	else {
		root->right = deleteNode(root->right, value);
	}

    if(value == root->data) {
		if (root->left == NULL && root->right == NULL) {
			root->data = NULL;
			free(root);
			Node* nullNode = NULL;
			return nullNode;
		}

		if (root->left != NULL && root->right == NULL) {
			Node* rootRight = root->right;
			free(root);
			return rootRight;
		}

		if (root->left == NULL && root->right != NULL) {
			Node* rootLeft = root->left;
			free(root);
			return rootLeft;
		}

		if (root->right != NULL && root->left != NULL) {
			int succNodeValue = getInorderSucc(root->right);
			root->data = succNodeValue;
			root->right = deleteNode(root->right, succNodeValue);
			return root;
		}		
    }
}

int getInorderSucc(Node* root) {
	if (root != NULL) {
		if (root->left == NULL) {
			return root->data;
		}
		else {
			getInorderSucc(root->left);
		}
	}
}

void printSubtree(Node *N)
{
    if(N != NULL) {
        printSubtree(N->left);
        printf("%d \n",N->data);
        printSubtree(N->right);
    }
}
int countNodes(Node *N)
{
    int numNodes = 0;
    if(N != NULL) {
        numNodes = 1 + countNodes(N->left) + countNodes(N->right);
    }
    return numNodes;
}

Node* freeSubtree(Node *N)
{
	printSubtree(N);
	if (N != NULL) {
		if (N->left == NULL && N->right == NULL) {
			free(N);
			return (Node*) NULL;
		}
		if (N->left != NULL) {
			freeSubtree(N->left);
		}
		if (N->right != NULL) {
			freeSubtree(N->right);
		}
	}
	return (Node*) NULL;
}

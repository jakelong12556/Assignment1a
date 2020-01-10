package com.bham.pij.assignments.assignment1;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;

public class EmailAddressFinder {

    private static ArrayList<String> emailAddresses;

    public static void main(String[] args) {
        emailAddresses = new ArrayList<String>();
        EmailAddressFinder eaf = new EmailAddressFinder();
        eaf.run();
        System.out.println("Email addresses found: " + emailAddresses.size());
    }

    public void run() {

        BufferedReader reader = null;

        try {
            reader = new BufferedReader(new FileReader("corrupteddb"));
            String input = "";

            PrintWriter pw = new PrintWriter("eaf");

            while ((input = reader.readLine()) != null) {

                input = input.trim();

                ArrayList<String> temp = new ArrayList<String>();

                temp = findEmailAddresses(input);

                for (String t: temp) {
                    emailAddresses.add(t);
                }
            }

            pw.close();
            reader.close();
        }

        catch (IOException e) {
            e.printStackTrace();
        }
    }

    public ArrayList<String> findEmailAddresses(String input) {
        ArrayList<String> list = new ArrayList<String>();
        input = input + ".com.jp.ro.uk.de.net"; //makes domainFinder not return -1/lowest value, nulling search prematurely

        while (input.length() > 0) {
            input = "$" + input; //force invalid char between emails, separating them
            int testInt = domainFinder(input);
            String testString = input.substring(0,testInt);
            int lastAt = testString.lastIndexOf('@'); //find last @ sign, @ + char + topLvDomain, check char.
            if (lastAt != -1) {
                String domain = testString.substring(lastAt + 1); //starting after the @ sign.
                String local = testString.substring(0,lastAt);
                if (domainChecker(domain) == true) {
                    if (localPosition(local) != 0) {
                        String email = testString.substring(localPosition(local), testInt);
                        list.add(email);
                        input = input.substring(testInt); //makes a new substring that removes the teststring in all scenarios
                    } else {
                        input = input.substring(testInt);
                    }
                } else {
                    input = input.substring(testInt);
                }
            } else {
                input = input.substring(testInt);
            }
        }
        return list;
    }

    public static int localPosition(String input) {
        int dotCounter = 0;
        int testStrLength = input.length();//if 0 after check = invalid, else return valid email
        final int testStrLength1 = input.length();
        int strLength = 0;
        int result = 0;

        for (int i = testStrLength - 1; i > 0; i--) { //checks backwards
            char cycle = input.charAt(i);
            if (dotCounter == 0) {
                if (Character.isLetterOrDigit(cycle) == true) {
                    strLength--;
                } else {
                    if (cycle == '_') {
                        strLength--;
                    } else {
                        if (cycle == '.') {
                            strLength--;
                            dotCounter++;
                        } else {
                            break;
                        }
                    }
                }
            } else {
                if (Character.isLetterOrDigit(cycle) == true) {
                    strLength--;
                } else {
                    if (cycle == '_') {
                        strLength--;
                    } else {
                        break;
                    }
                }
            }
        }
        if (strLength != 0) {
            result = testStrLength1 + strLength;
        } else { // imbed boolean, if 0 = invalid
            result = 0;
        }
        return result;

    }
    public static boolean domainChecker(String input) { //check between the @ and topLvDomain for valid inputs
        boolean outcome = false;
        int dotCounter = 0;
        final int MaxDotNo = 3;
        int testStrLength = input.length();

        for (int i = 0; i < testStrLength; i++) {
            char cycle = input.charAt(i);
            if (dotCounter <= MaxDotNo) {
                if (Character.isLetter(cycle) == true && Character.isLowerCase(cycle) == true) {
                    outcome = true;
                } else {
                    if (cycle == '.') {
                        outcome = true;
                        dotCounter++;
                    } else {
                        outcome = false;
                        break;
                    }
                }
            } else {
                outcome = false;
                break;
            }
        }
        return outcome;
    }

    public static int domainFinder(String input) { //return lowest topLvDomain from int array, avoid email wrapping
        final int addDomain4 = 4;//move indexOf to the end of the topLvDomain
        final int addDomain3 = 3;

        ArrayList<Integer> topLvDomain = new ArrayList<Integer>();

        topLvDomain.add(input.indexOf(".net") + addDomain4);
        topLvDomain.add(input.indexOf(".com") + addDomain4);
        topLvDomain.add(input.indexOf(".jp")  + addDomain3);
        topLvDomain.add(input.indexOf(".uk")  + addDomain3);
        topLvDomain.add(input.indexOf(".de")  + addDomain3);
        topLvDomain.add(input.indexOf(".ro")  + addDomain3);

        Integer minIndex = Collections.min(topLvDomain);

        return minIndex;
    }
}

#include <iostream>
#include <vector>
#include <string>
#include <cmath>
#include <fstream>
using namespace std;

string iToBin(int val,int n){
    string res="";
    while(val){
        res=to_string(val%2)+res;
        val/=2;
        n--;
    }
    for(int i=0;i<n;i++){
        res='0'+res;
    }
    return res;
}

void writeReg(ostream &o, vector <string> q, string s){
    int n=q.size();
    if(s!="jk"){
        for(int i=n-1;i>=0;i--){
            o<<"register "<<s<<"ff"<<i<<"(clk,"<<q[n-i-1]<<",Q["<<to_string(i)<<"]);\n";
        }
        return;
    }
    for(int i=n/2-1;i>=0;i--){
        o<<"register "<<s<<"ff"<<i<<"(clk,"<<q[n/2-i-1]<<","<<q[n-i-1]<<",Q["<<i<<"]);\n";
    }
}

void writeModule(ostream &o,int no,int nf){
    o<<"module counter(input clk,output ["<<no-1<<":0] O);\nwire ["<<nf-1<<":0] Q;\n";
}

int binToDec(string s){
    int r=0;
    for(int i=s.size()-1;i>=0;i--){
        r+=pow(2,i)*(s[s.size()-i-1]-48);
    }
    return r;
}

int* nextBl(int* bl,int x,int t){
    int* res=new int[4];
    for(int i=0;i<4;i++)
        res[i]=bl[i];
    if(bl[2]!=-1){
        int w=bl[3]-bl[2]+1, l=bl[1]-bl[0]+1;
        if(t==0){
            if(l==1&&bl[1]==x-1){
                res[0]=bl[2];
                res[1]=bl[3];
                res[2]=-1;
                res[3]=0;
                return res;
            }
            res[1]+=l;
            return res;
        }
        if(t==1){
            if(w==1&&bl[3]==x-1){
                res[2]=-1;
                res[3]=1;
                return res;
            }
            res[3]+=w;
            return res;
        }
        if(t==2){
            if(l==1&&bl[0]==1){
                res[0]=bl[2];
                res[1]=bl[3];
                res[2]=-1;
                res[3]=0;
                return res;
            }
            res[0]-=l;
            return res;
        }
        if(w==1&&bl[2]==1){
            res[2]=-1;
            res[3]=1;
            return res;
        }
        res[2]-=w;
        return res;
    }
    int l=res[1]-res[0]+1;
    if(bl[3]==0){
        if(t==1){
            if(l==1&&res[0]==x-1){
                res[3]=-1;
                return res;
            }
            res[1]+=l;
            return res;
        }
        if(l==1&&res[0]==1){
            res[3]=-1;
            return res;
        }
        res[0]-=l;
        return res;
    }
    if(t==0){
        if(l==1&&res[0]==x-1){
            res[3]=-1;
            return res;
        }
        res[1]+=l;
        return res;
    }
    if(l==1&&res[0]==1){
        res[3]=-1;
        return res;
    }
    res[0]-=l;
    return res;
}

bool validBl(vector<vector<string> > k,int* bl){
    int c=k[0][1].size()*2+1, r=k[1][0].size()*2+1;
    if(bl[3]==-1) return 0;
    if(bl[2]!=-1){
        if(bl[0]<1||bl[1]>=c||bl[2]<1||bl[3]>=r) return 0;
        for(int i=bl[0];i<=bl[1];i++){
            for(int j=bl[2];j<=bl[3];j++){
                if(k[j][i]=="0") return 0;
            }
        }
        return 1;
    }
    if(bl[3]==0){
        if(bl[0]<1||bl[1]>=r) return 0;
        for(int i=bl[0];i<=bl[1];i++){
            if(k[i][1]=="0"||k[i][c-1]=="0") return 0;
        }
        return 1;
    }
    if(bl[0]<1||bl[1]>=c) return 0;
    for(int i=bl[0];i<=bl[1];i++){
        if(k[1][i]=="0"||k[r-1][i]=="0") return 0;
    }
    return 1;
}

string generateBlockExp(vector<vector<string> > k,string ex[],int* bl){
    string res="";
    int a0=k[0][1].size(), a1=k[1][0].size();
    int r=a1*2+1, c=a0*2+1;
    if(bl[2]!=-1){
        if(bl[1]-bl[0]==c-2&&bl[3]-bl[2]==r-2){
            return "1";
        }
        for(int i=0;i<a0;i++){
            int t=1;
            char x=k[0][bl[0]][i];
            for(int j=bl[0]+1;j<=bl[1];j++){
                if(k[0][j][i]!=x){
                    t=0;
                    break;
                }
            }
            if(t){
                if(x=='0'){
                    res+="~"+ex[a0+a1-i-1]+"&";
                }
                else{
                    res+=ex[a0+a1-i-1]+"&";
                }
            }
        }
        for(int i=0;i<a1;i++){
            int t=1;
            char x=k[bl[2]][0][i];
            for(int j=bl[2]+1;j<=bl[3];j++){
                if(k[j][0][i]!=x){
                    t=0;
                    break;
                }
            }
            if(t){
                if(x=='0'){
                    res+="~"+ex[a1==1?0:!i]+"&";
                }
                else{
                    res+=ex[a1==1?0:!i]+"&";
                }
            }
        }
        res.pop_back();
        if(res.size()==5||res.size()==4) return res;
        return "("+res+")";
    }
    if(bl[3]==0){
        if(bl[1]-bl[0]==r-2&&c==3) return "1";
        for(int i=0;i<a0;i++){
            if(k[0][1][i]!=k[0][c-1][i]) continue;
            if(k[0][1][i]=='0'){
                res+="~"+ex[a0+a1-i-1]+"&";
            }
            else{
                res+=ex[a0+a1-i-1]+"&";
            }
        }
        for(int i=0;i<a1;i++){
            int t=1;
            char x=k[bl[0]][0][i];
            for(int j=bl[0]+1;j<=bl[1];j++){
                if(k[j][0][i]!=x){
                    t=0;
                    break;
                }
            }
            if(t){
                if(x=='0'){
                    res+="~"+ex[a1==1?0:!i]+"&";
                }
                else{
                    res+=ex[a1==1?0:!i]+"&";
                }
            }
        }
        res.pop_back();
        if(res.size()==5||res.size()==4) return res;
        return "("+res+")";
    }
    if(bl[3]==1){
        if(bl[1]-bl[0]==c-2&&r==3) return "1";
        for(int i=0;i<a0;i++){
            int t=1;
            char x=k[0][bl[0]][i];
            for(int j=bl[0]+1;j<=bl[1];j++){
                if(k[0][j][i]!=x){
                    t=0;
                    break;
                }
            }
            if(t){
                if(x=='0'){
                    res+="~"+ex[a0+a1-i-1]+"&";
                }
                else{
                    res+=ex[a0+a1-i-1]+"&";
                }
            }
        }
        for(int i=0;i<a1;i++){
            if(k[1][0][i]!=k[r-1][0][i]) continue;
            if(k[1][0][i]=='0'){
                res+="~"+ex[a1==1?0:!i]+"&";
            }
            else{
                res+=ex[a1==1?0:!i]+"&";
            }
        }
        res.pop_back();
        if(res.size()==5||res.size()==4) return res;
        return "("+res+")";
    }
    if(r==3&&c==3) return "1";
    for(int i=0;i<a0;i++){
        if(k[0][1][i]!=k[0][c-1][i]) continue;
        if(k[0][1][i]=='0'){
            res+="~"+ex[a0+a1-i-1]+"&";
        }
        else{
            res+=ex[a0+a1-i-1]+"&";
        }
    }
    for(int i=0;i<a1;i++){
        if(k[1][0][i]!=k[r-1][0][i]) continue;
        if(k[1][0][i]=='0'){
            res+="~"+ex[a1==1?0:!i]+"&";
        }
        else{
            res+=ex[a1==1?0:!i]+"&";
        }
    }
    res.pop_back();
    if(res.size()==5||res.size()==4) return res;
    return "("+res+")";
}

void fillCheck(vector<vector<string> >k,vector< vector<int> >& ch,int* bl){
    int c=ch[0].size(), r=ch.size();
    if(bl[2]!=-1){
        for(int i=bl[0];i<=bl[1];i++){
            for(int j=bl[2];j<=bl[3];j++){
                if(k[j][i]=="1") ch[j][i]=1;
            }
        }
        return;
    }
    if(bl[3]==-1){
        if(k[1][1]=="1") ch[1][1]=1;
        if(k[r-1][1]=="1") ch[r-1][1]=1;
        if(k[1][c-1]=="1") ch[1][c-1]=1;
        if(k[r-1][c-1]=="1") ch[r-1][c-1]=1;
        return;
    }
    if(bl[3]==0){
        for(int i=bl[0];i<=bl[1];i++){
            if(k[i][1]=="1") ch[i][1]=1;
            if(k[i][c-1]=="1") ch[i][c-1]=1;
        }
        return;
    }
    for(int i=bl[0];i<=bl[1];i++){
        if(k[1][i]=="1") ch[1][i]=1;
        if(k[r-1][i]=="1") ch[r-1][i]=1;
    }
}

class truthTable 
{
    private:
    int numOut;
    vector <int> input; 
    vector <string> q,x,qf,o;
    
    public:
    truthTable(int nO,vector <int> i);
    int numFf();
    vector<string> getQ() {return q;}
    vector<string> getX() {return x;}
    vector<string> getO() {return o;}
    void qqfTable();
    void dTable();
    void tTable();
    void jkTable();
    void oTable();
};

truthTable::truthTable(int nO,vector <int> i){
    numOut=nO;
    input=i;    
}

int truthTable::numFf(){
    int i=1,numMod=input.size();
    while(pow(2,i)<numMod) i++;
    return i;
}

void truthTable::qqfTable(){
    int nf=this->numFf(),numMod=input.size();
    for(int i=0;i<numMod;i++){
        q.push_back(iToBin(i,nf));
        if(i!=numMod-1) qf.push_back(iToBin(i+1,nf));
        else qf.push_back(iToBin(0,nf));
    }
}

void truthTable::dTable(){
    int nf=this->numFf(),numMod=input.size();
    for(int i=0;i<numMod;i++){
        x.push_back(qf[i]);
    }
}

void truthTable::tTable(){
    int nf=this->numFf(),numMod=input.size();
    vector<string> y(numMod,"");
    x=y;
    for(int i=0;i<nf;i++){
        for(int j=0;j<numMod;j++){
            if(q[j][i]==qf[j][i]){
                x[j].push_back('0');
            }
            else x[j].push_back('1');
        }
    }
}

void truthTable::jkTable(){
    int nf=this->numFf(),numMod=input.size();
    string s(nf*2,'0');
    vector<string> y(numMod,s);
    x=y;
    for(int i=0;i<nf;i++){
        for(int j=0;j<numMod;j++){
            if(q[j][i]=='0'&&qf[j][i]=='0'){
                x[j][i]='0';
                x[j][i+nf]='x';
            }
            else if(q[j][i]=='0'&&qf[j][i]=='1'){
                x[j][i]='1';
                x[j][i+nf]='x';
            }
            else if(q[j][i]=='1'&&qf[j][i]=='0'){
                x[j][i]='x';
                x[j][i+nf]='1';
            }
            else{
                x[j][i]='x';
                x[j][i+nf]='0';
            }
        }
    }
}

void truthTable::oTable(){
    int numMod=input.size();
    for(int i=0;i<numMod;i++){
        o.push_back(iToBin(input[i],numOut));
    }
}

class wires
{
    private:
    vector <string> q,x,res;
    
    public:
    wires() {
        q.push_back("0");
        x.push_back("0");
    }
    wires(vector <string> q0,vector<string> x0);
    string generateEx(int n);
    vector<string> getResult();
};

wires::wires(vector <string> q0,vector<string> x0){
    q=q0;
    x=x0;
}

string wires::generateEx(int n){
    int numMod=q.size(), numFf=q[0].size();
    vector<string> expr;
    string exQ[numFf];
    string xff="";
    string result="";
    int row=pow(2,numFf/2)+1, col=pow(2,(numFf+1)/2)+1;
    vector<string> s(col,"0");
    vector<vector<string> > kMap(row,s);
    vector< vector<int> > check(row,vector<int> (col,0));
    for(int i=0;i<numMod;i++){
        xff+=x[i][n];
    }
    for(int i=0;i<numFf;i++){
        exQ[i]="Q["+to_string(i)+"]";
    }
    for(int i=1;i<col;i++){
        kMap[0][i]=iToBin(i-1,col/2);
    }
    if(numFf>2) swap(kMap[0][3],kMap[0][4]);
    for(int i=1;i<row;i++){
        kMap[i][0]=iToBin(i-1,row/2);
    }
    if(numFf>3) swap(kMap[3][0],kMap[4][0]);
    //insert data into Kmap
    for(int i=1;i<row;i++){
        for(int j=1;j<col;j++){
            if(binToDec(kMap[0][j]+kMap[i][0])>numMod-1){
                kMap[i][j]="x";
            }
        }
    }
    for(int i=0;i<numMod;i++){
        for(int j=1;j<row;j++){
            int t=0;
            for(int t=1;t<col;t++){
                if(kMap[j][t]!="0") continue;
                if(kMap[0][t]+kMap[j][0]==q[i]){
                    kMap[j][t]=xff[i];
                    t=1;
                    break;
                }
            }
            if(t) break;
        }
    }
    //draw block and generate expression
    for(int i=1;i<row;i++){
        for(int j=1;j<col;j++){
            if(kMap[i][j]!="1"||check[i][j]==1) continue;
            int* block=new int[4];
            block[0]=j; block[1]=j; block[2]=i; block[3]=i;
            //draw block
            while(validBl(kMap,nextBl(block,col,0))||validBl(kMap,nextBl(block,row,1))||validBl(kMap,nextBl(block,col,2))||validBl(kMap,nextBl(block,col,2))){
                if(validBl(kMap,nextBl(block,col,0))&&(block[3]!=0||block[2]!=-1)){
                    block=nextBl(block,col,0);
                }
                else if(validBl(kMap,nextBl(block,row,1))&&(block[2]!=-1||block[3]!=1)){
                    block=nextBl(block,row,1);
                }
                else if(validBl(kMap,nextBl(block,col,2))&&(block[3]!=0||block[2]!=-1)){
                    block=nextBl(block,col,2);
                }
                else{
                    block=nextBl(block,col,3);
                }
            }
            //Generate expression from block
            expr.push_back(generateBlockExp(kMap,exQ,block));
            fillCheck(kMap,check,block);
        }
    }
    if(expr.size()==0) return "0";
    if(expr.size()==1&&expr[0].size()>5){
        expr[0].erase(expr[0].begin());
        expr[0].pop_back();
        return expr[0];
    }
    for(int i=0;i<expr.size();i++){
        result+=expr[i]+"|";
    }
    result.pop_back();
    return result;
}

vector<string> wires::getResult(){
    for(int i=0;i<x[0].size();i++){
        res.push_back(generateEx(i));
    }
    return res;
}

int main(){
    int numMod,numFf,numOut=0,a,choose;
    vector<int> input;
    cout<<"Nhap so Mod (>2,<16): ";
    cin>>numMod;
    cout<<"Nhap chuoi so: ";
    int max=0;
    for(int i=0;i<numMod;i++){
        cin>>a;
        input.push_back(a);
        if(max<a) max=a;
    }
    while(max){
        numOut++;
        max/=2;
    }
    cout<<"Chon loai Flip Flop:\n1.D Flip Flop\n2.T Flip Flop\n3.JK Flip Flop\n";
    cout<<"Nhap lua chon (1,2 hoac 3): ";
    cin>>choose;
    truthTable T(numOut,input);
    numFf=T.numFf();
    T.oTable();
    T.qqfTable();
    wires O(T.getQ(),T.getO());
    wires Q;
    ofstream hdl("result.txt");
    //make output string
    string o="";
    for(int i=numOut-1;i>=0;i--){
        o+="assign O["+to_string(i)+"]="+O.getResult()[numOut-i-1]+";\n";
    }
    o+="endmodule";
    switch(choose){
        case 1:
            T.dTable();
            Q=wires (T.getQ(),T.getX());
            hdl<<"module register(input clk,input d,output reg q);\nalways @(posedge clk)\n";
            hdl<<"begin\n\tq<=d;\nend\nendmodule\n\n";
            writeModule(hdl,numOut,numFf);
            writeReg(hdl,Q.getResult(),"d");
            hdl<<o;
            break;
        case 2:
            T.tTable();
            Q=wires (T.getQ(),T.getX());
            hdl<<"module register(input clk,input t,output reg q);\nalways @(posedge clk)\n";
            hdl<<"begin\n\tq<=t^q;\nend\nendmodule\n\n";
            writeModule(hdl,numOut,numFf);
            writeReg(hdl,Q.getResult(),"t");
            hdl<<o;
            break;
        case 3:
            T.jkTable();
            Q=wires (T.getQ(),T.getX());
            hdl<<"module register(input clk,input j,input k,output reg q);\nalways @(posedge clk)\n";
            hdl<<"case ({j,k})\n\t2'b00 :  q <= q;\n\t2'b01 :  q <= 0;\n\t2'b10 :  q <= 1;\n\t2'b11 :  q <= ~q;\nendcase\nendmodule\n\n";
            writeModule(hdl,numOut,numFf);
            writeReg(hdl,Q.getResult(),"jk");
            hdl<<o;
    }
    system("pause");
    return 0;
}
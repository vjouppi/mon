/*-------------------------------
 *
 * fd2set.c
 * Timo Rossi 1991, 1994
 *
 * convert fd-file to a monitor script file
 * that defines function offsets
 *
 *-------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define EXIT_OK 0
#define EXIT_ERR 10

char line[256];
char temp[101];

char *getnumber(char *,int *);
char *gettoken(char *,char *,int);
char *skipspaces(char *);
char *basename(char *);

main(int argc, char *argv[])
{
  FILE	*fdfile,*outfile;
  char	*p;
  int	x;
  int	bias = -30;	/* default first function offset */

  if(argc!=2)
    {
      printf("USAGE: fd2set fdfilename\n");
      exit(EXIT_ERR);
    }

  if((fdfile=fopen(argv[1],"r"))==NULL)
    {
      printf("Can't open FD file.\n");
      exit(EXIT_ERR);
    }

  strcpy(line,basename(argv[1]));
  if(p=strchr(line,'_'))	*p='\0';	/* cut string at '_' */
  strcat(line,".set");
  if((outfile=fopen(line,"w"))==NULL)
    {
      printf("Can't open output file '%s'.\n",line);
      fclose(fdfile);
      exit(EXIT_ERR);
    }

  while(fgets(line, 200, fdfile))
    {
      p=line;
      switch(*p)
	{
	  case '\n': case '\0':	/* empty line	*/
	  case '*':			/* comment	*/
	    break;

	  case '#':			/* command	*/
	    p+=2;	/* skip #'s */
	    p=gettoken(p,temp,100);	/* get command name */
	    if(!strcmp(temp,"bias"))
	      {
		p=getnumber(p,&bias);
		bias=-bias;
	      }
	    break;

	  default:			/* function entry    */
	    p=gettoken(p,temp,100);	/* get function name */
	    strcpy(line,"set ");
	    strcat(line,"_LVO");
	    strcat(line,temp);
	    x=strlen(line);
	    if(x>=24)	x=23;	/* at least one TAB  */
	    while(x<24)
	      {
		strcat(line,"\t");
		x=(x & -8) + 8;
	      }
	    sprintf(temp," -$%x", -bias);
	    strcat(line,temp);
	    fprintf(outfile,"%s\n",line);
	    bias-=6;
	    break;
	}
    }

  fclose(fdfile);
  fclose(outfile);
  exit(EXIT_OK);
}

/*
 * get a decimal number from a string pointed by p
 * put number in *num, return updated string pointer
 */
char *getnumber(char *p, int *num)
{
  int val=0;
  short	sign=0;

  p=skipspaces(p);
  if(*p=='-')
    {
      sign=1;
      p++;
    }
  while(isdigit(*p))
    val=10*val+((*p++)-'0');

  *num=( sign ? -val : val );
  return(p);
}

/*
 * get a token (alphanumeric characters), put it in tok[]
 * return updated string pointer
 * len is length of tok[] buffer (excluding NULL)
 */
char *gettoken(char *p, char *tok, int len)
{
  p=skipspaces(p);
  while(len && (isalnum(*p) || *p=='_') )
    {
      *tok++=*p++;
      len--;
    }
  *tok='\0';	/* end of string */
  return(p);
}

/*
 * skip spaces
 */
char *skipspaces(char *p)
{
  while(isspace(*p))
    p++;

  return(p);
}

/*
 * return pointer to file name part of [device:][{directory/}]filename
 */
char *basename(char *name)
{
  char *p;

  if((p = strrchr(name, '/')) != NULL ||
     (p = strrchr(name, ':')) != NULL)
    return p+1;

  return name;
}

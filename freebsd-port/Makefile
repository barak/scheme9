# New ports collection makefile for:	s9fes
# Date created:				23 February 2009
# Whom:					Nils M Holm <nmh@t3x.org>
#
# $FreeBSD: ports/lang/s9fes/Makefile,v 1.14 2010/09/11 19:40:17 makc Exp $
#

PORTNAME=	s9fes
PORTVERSION=	20101004
CATEGORIES=	lang scheme
MASTER_SITES=	http://www.t3x.org/misc/

MAINTAINER=	nmh@t3x.org
COMMENT=	A portable Scheme interpreter with a Unix interface
LICENSE=	public-domain

WRKSRC=		${WRKDIR}/s9

MAN1=		s9.1
MANCOMPRESSED=	yes

post-patch:
	${REINPLACE_CMD} -e "s,PREFIX=.*,PREFIX=${PREFIX}," ${WRKSRC}/Makefile
	${REINPLACE_CMD} -e "s,LIBDIR=.*,LIBDIR=${DATADIR}," ${WRKSRC}/Makefile

.include <bsd.port.mk>
